# 05_precos.R
# Modelo C: o choque apareceu nos precos?
# Inflacao mensal da RM de Belem menos a do Brasil, por item do IPCA (SIDRA, tabela 7060),
# regredida nas fases da COP30 com dummies de mes (MQO, erros de Newey-West).
#
# O download e feito ano a ano: a tabela inteira de uma vez passa do limite do SIDRA.
#
# saida: dados/processados/precos_belem_brasil.csv
#        resultados/tabelas/  tabela4_precos.tex/.html, precos_por_fase.csv
#        resultados/figuras/  figura4_precos.png

pacotes <- c("data.table", "sidrar", "sandwich", "modelsummary", "ggplot2")
novos <- setdiff(pacotes, rownames(installed.packages()))
if (length(novos)) install.packages(novos)
invisible(lapply(pacotes, library, character.only = TRUE))

if (!file.exists("cop30.Rproj")) stop("abra o projeto cop30.Rproj antes de rodar")
PROC <- "dados/processados"; TAB <- "resultados/tabelas"; FIG <- "resultados/figuras"

INICIO <- as.Date("2022-01-01")
FIM    <- as.Date("2026-07-01")
FASES <- list(prep   = c("2024-07-01", "2025-10-01"),
              evento = c("2025-11-01", "2025-11-01"),
              desmob = c("2025-12-01", "2026-07-01"))
# itens do IPCA (parte do nome, sem o codigo numerico da frente)
ITENS <- c("Índice geral", "Alimentação fora do domicílio", "Hospedagem", "Aluguel residencial")

# ---- download ----------------------------------------------------------
baixa <- function(territorio, ano) {
  ultimo <- if (ano == as.integer(format(FIM, "%Y"))) format(FIM, "%Y%m") else paste0(ano, "12")
  api <- sprintf("/t/7060/%s/v/63/p/%d01-%s/c315/all", territorio, ano, ultimo)
  message("sidra ", api)
  x <- as.data.table(sidrar::get_sidra(api = api))
  col_item <- grep("item", names(x), ignore.case = TRUE, value = TRUE)
  col_item <- col_item[!grepl("C.digo", col_item)][1]
  col_mes  <- grep("^M.s \\(C.digo\\)$", names(x), value = TRUE)[1]
  x <- x[, .(item = trimws(sub("^[0-9.]+", "", get(col_item))),
             competencia = as.Date(paste0(get(col_mes), "01"), "%Y%m%d"),
             valor = as.numeric(Valor))]
  unique(x)      # o mesmo nome aparece em mais de um nivel da classificacao (subgrupo, item, subitem)
}
anos <- as.integer(format(INICIO, "%Y")):as.integer(format(FIM, "%Y"))
bel <- rbindlist(lapply(anos, function(a) baixa("n7/1501", a)))    # RM de Belem
br  <- rbindlist(lapply(anos, function(a) baixa("n1/all", a)))     # Brasil

faltam <- setdiff(ITENS, unique(bel$item))
if (length(faltam)) message("itens sem dado para a RM de Belem (ficam de fora): ", paste(faltam, collapse = "; "))
precos <- merge(bel[item %in% ITENS], br[item %in% ITENS], by = c("item", "competencia"),
                suffixes = c("_belem", "_brasil"))
precos <- precos[!is.na(valor_belem) & !is.na(valor_brasil)]
precos <- unique(precos)            # garante uma linha por item e mes (o SIDRA pode repetir linhas)
precos[, dif := valor_belem - valor_brasil]                         # p.p.
precos[, mes_f := factor(as.integer(format(competencia, "%m")))]
precos[, fase := "referencia"]
for (f in names(FASES)) {
  lim <- as.Date(FASES[[f]])
  precos[, (f) := as.integer(competencia >= lim[1] & competencia <= lim[2])]
  precos[get(f) == 1, fase := f]
}
fwrite(precos, file.path(PROC, "precos_belem_brasil.csv"))

# ---- descritiva: inflacao media mensal por fase ------------------------
desc <- precos[, .(belem = round(mean(valor_belem), 2), brasil = round(mean(valor_brasil), 2),
                   diferenca = round(mean(dif), 2), meses = .N), by = .(item, fase)]
print(desc)
fwrite(desc, file.path(TAB, "precos_por_fase.csv"))

# ---- modelo C ----------------------------------------------------------
vc_nw <- function(m) sandwich::NeweyWest(m, prewhite = FALSE, adjust = TRUE)
modC <- lapply(split(precos, precos$item), function(d) lm(dif ~ prep + evento + desmob + mes_f, data = d))
for (ext in c("html", "tex")) {
  modelsummary(modC, vcov = vc_nw, stars = c("*" = .1, "**" = .05, "***" = .01),
               coef_map = c(prep = "Preparação", evento = "Evento (nov/2025)", desmob = "Desmobilização"),
               gof_map = c("nobs", "r.squared"),
               title = "Modelo C: inflação mensal da RM de Belém menos a do Brasil (p.p.), por item do IPCA",
               notes = "MQO com dummies de mês; erros-padrão de Newey-West entre parênteses.",
               output = file.path(TAB, paste0("tabela4_precos.", ext)))
}
res <- rbindlist(lapply(names(modC), function(nm) {
  m <- modC[[nm]]; ep <- sqrt(diag(vc_nw(m)))
  data.table(item = nm, fase = c("prep", "evento", "desmob"),
             efeito_pp = round(coef(m)[c("prep", "evento", "desmob")], 3),
             erro_padrao = round(ep[c("prep", "evento", "desmob")], 3))
}))
res[, t := round(efeito_pp / erro_padrao, 2)]
fwrite(res, file.path(TAB, "precos_modeloC.csv"))

# o evento e um mes so: a dummy se ajusta perfeitamente e o t nao serve como teste.
# Teste usado no texto: posicao de nov/2025 entre todos os meses da serie (placebo no tempo)
# e nivel de precos de Belem em relacao ao Brasil (jun/2024 = 100).
setorder(precos, item, competencia)
precos[, rel := cumprod((1 + valor_belem / 100) / (1 + valor_brasil / 100)), by = item]
precos[, indice_relativo := 100 * rel / rel[competencia == as.Date("2024-06-01")], by = item]
teste <- precos[, {
  nov <- dif[competencia == as.Date("2025-11-01")]
  pre <- dif[competencia < as.Date("2025-11-01")]
  .(dif_nov2025 = round(nov, 2), dp_antes = round(sd(pre), 2), maior_antes = round(max(abs(pre)), 2),
    posicao = sum(abs(dif) >= abs(nov)), meses = .N, p_tempo = round(sum(abs(dif) >= abs(nov)) / .N, 3),
    indice_out2025 = round(indice_relativo[competencia == as.Date("2025-10-01")], 1),
    indice_nov2025 = round(indice_relativo[competencia == as.Date("2025-11-01")], 1),
    indice_dez2025 = round(indice_relativo[competencia == as.Date("2025-12-01")], 1),
    indice_jul2026 = round(indice_relativo[competencia == as.Date("2026-07-01")], 1))
}, by = item]
print(teste)
fwrite(teste, file.path(TAB, "precos_teste_nov2025.csv"))

# ---- figura ------------------------------------------------------------
g <- ggplot(precos, aes(competencia, dif)) +
  annotate("rect", xmin = as.Date(FASES$prep[1]), xmax = as.Date(FASES$evento[1]),
           ymin = -Inf, ymax = Inf, fill = "grey92") +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_line() +
  geom_vline(xintercept = as.Date(FASES$evento[1]), linetype = 3) +
  facet_wrap(~ item, scales = "free_y", ncol = 2) +
  labs(x = NULL, y = "Inflação mensal: RM de Belém menos Brasil (p.p.)",
       caption = paste0("Fonte: IBGE, IPCA, SIDRA tabela 7060 (https://sidra.ibge.gov.br/tabela/7060): variável 63 ",
                        "(variação mensal, %), níveis territoriais Brasil e Região Metropolitana de Belém (1501),\n",
                        "itens selecionados na classificação 315, jan/2022 a jul/2026; diferença calculada mês a mês. ",
                        "Faixa: preparação; linha pontilhada: nov/2025.")) +
  theme_minimal() + theme(plot.caption = element_text(hjust = 0, size = 7))
ggsave(file.path(FIG, "figura4_precos.png"), g, width = 9, height = 5.5, dpi = 200)

cat("\nModelo C (efeito em p.p. da inflacao mensal; t = efeito / erro-padrao)\n"); print(res)

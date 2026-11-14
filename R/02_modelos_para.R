# 02_modelos_para.R
# Painel de municipios do Para (CNAE 55 + 56) e modelos do paper:
#   modelo A - serie de cada grupo (Belem, RM, interior), MQO + Newey-West, controles macro
#   modelo B - painel de municipios com efeitos fixos (efeito local x transbordamento)
#   placebo  - cada municipio do interior no lugar de Belem
#   estudo de evento (tendencias antes da COP) e efeito acumulado em vagas
#   modelo C (precos, IPCA) e teste ADF
#
# rodar depois do 01_base_para.R
# entrada: dados/brutos (CAGED), dados/processados/base_cop30_para.csv, BCB-SGS, SIDRA 7060
# saida:   dados/processados/  painel_municipios.csv, macro.csv, base_helder.xlsx,
#                              event_study_para.csv, precos_belem_brasil.csv
#          resultados/tabelas/ tabela1 a tabela4 (.tex/.html), placebo_belem.csv,
#                              efeitos_em_vagas.csv, pretendencia_para.csv,
#                              efeito_acumulado_para.csv, teste_adf.csv
#          resultados/figuras/ figura1_grupos.png, figura2_event_study_para*.png,
#                              figura3_acumulado_para*.png

pacotes <- c("data.table", "archive", "fixest", "rbcb", "sandwich", "writexl", "tseries", "sidrar",
             "lmtest", "modelsummary", "ggplot2")
novos <- setdiff(pacotes, rownames(installed.packages()))
if (length(novos)) install.packages(novos)
invisible(lapply(pacotes, library, character.only = TRUE))
options(timeout = 1800)

if (!file.exists("cop30.Rproj")) stop("abra o projeto cop30.Rproj antes de rodar")
source("R/funcoes.R")

BRUTOS <- "dados/brutos"
INTER  <- "dados/intermediarios/agregados_mun"
PROC   <- "dados/processados"
TAB    <- "resultados/tabelas"
FIG    <- "resultados/figuras"
for (p in c(BRUTOS, INTER, PROC, TAB, FIG)) dir.create(p, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(file.path(PROC, "base_cop30_para.csv")))
  stop("falta dados/processados/base_cop30_para.csv; rodar o 01_base_para.R antes")

# parametros
INICIO   <- as.Date("2022-01-01")
FIM      <- as.Date("2026-07-01")
UF_ALVO  <- 15L
DIVISOES <- c("55", "56")
MIN_ADM  <- 5          # sai municipio com menos de 5 admissoes/mes na referencia

BELEM    <- "150140"
RM_RESTO <- c("150080",  # Ananindeua
              "150442",  # Marituba
              "150150",  # Benevides
              "150635",  # Santa Barbara do Para
              "150650",  # Santa Izabel do Para
              "150240")  # Castanhal

REF   <- c("2022-01-01", "2024-06-01")   # periodo de referencia
FASES <- list(prep   = c("2024-07-01", "2025-10-01"),
              evento = c("2025-11-01", "2025-11-01"),
              desmob = c("2025-12-01", "2026-07-01"))

# ---- CAGED por municipio ---------------------------------------------
url_caged <- function(tipo, ano, mes) {
  sprintf("ftp://ftp.mtps.gov.br/pdet/microdados/NOVO%%20CAGED/%d/%d%02d/CAGED%s%d%02d.7z",
          ano, ano, mes, tipo, ano, mes)
}
achar_coluna <- function(nomes, padrao) {
  i <- grep(padrao, tolower(nomes), perl = TRUE)
  if (length(i) != 1) stop("coluna nao encontrada: ", padrao)
  nomes[i]
}

ler_caged_mun <- function(tipo, ano, mes) {
  cache <- file.path(INTER, sprintf("%s_%d%02d.rds", tipo, ano, mes))
  if (file.exists(cache)) return(readRDS(cache))

  arq7z <- file.path(BRUTOS, sprintf("CAGED%s%d%02d.7z", tipo, ano, mes))
  if (!file.exists(arq7z)) {
    ok <- tryCatch({ download.file(url_caged(tipo, ano, mes), arq7z, mode = "wb", quiet = TRUE); TRUE },
                   error = function(e) FALSE)
    if (!ok || !file.exists(arq7z) || file.size(arq7z) < 1000) {
      unlink(arq7z); message("indisponivel: CAGED", tipo, ano, sprintf("%02d", mes)); return(NULL)
    }
  }
  dir_tmp <- file.path(tempdir(), sprintf("mun_%s%d%02d", tipo, ano, mes))
  archive::archive_extract(arq7z, dir = dir_tmp)
  txt <- list.files(dir_tmp, pattern = "\\.txt$", full.names = TRUE,
                    ignore.case = TRUE, recursive = TRUE)[1]
  cab <- names(fread(txt, nrows = 0, sep = ";"))
  col <- c(competenciamov    = achar_coluna(cab, "^compet.*mov$"),
           uf                = achar_coluna(cab, "^uf$"),
           municipio         = achar_coluna(cab, "^munic"),
           subclasse         = achar_coluna(cab, "^subclasse$"),
           saldomovimentacao = achar_coluna(cab, "^saldomovim"))
  dt <- fread(txt, sep = ";", select = unname(col), colClasses = "character", showProgress = FALSE)
  setnames(dt, unname(col), names(col))
  dt <- dt[as.integer(uf) == UF_ALVO]
  dt[, divisao := substr(sprintf("%07d", as.integer(subclasse)), 1, 2)]
  dt <- dt[divisao %in% DIVISOES]
  dt[, `:=`(mov = as.integer(saldomovimentacao),
            mun = substr(municipio, 1, 6),
            competencia = as.Date(paste0(competenciamov, "01"), "%Y%m%d"))]
  agg <- dt[, .(adm = sum(mov == 1L), desl = sum(mov == -1L)), by = .(competencia, mun, divisao)]
  agg[, tipo := tipo]
  saveRDS(agg, cache)
  unlink(dir_tmp, recursive = TRUE)
  agg
}

meses <- seq(INICIO, FIM, by = "month")
lista <- list()
for (i in seq_along(meses)) {
  a <- as.integer(format(meses[i], "%Y")); m <- as.integer(format(meses[i], "%m"))
  message("caged ", format(meses[i], "%Y-%m"))
  for (tipo in c("MOV", "FOR", "EXC")) lista[[paste(tipo, a, m)]] <- ler_caged_mun(tipo, a, m)
}
mov <- rbindlist(lista)
mov[, sinal := fifelse(tipo == "EXC", -1L, 1L)]
cg <- mov[competencia >= INICIO & competencia <= FIM,
          .(adm = sum(sinal * adm), desl = sum(sinal * desl)),
          by = .(competencia, mun, divisao)]

# ---- painel balanceado municipio x mes -------------------------------
tot <- cg[, .(adm = sum(adm), desl = sum(desl)), by = .(competencia, mun)]
d55 <- cg[divisao == "55", .(adm_55 = sum(adm), desl_55 = sum(desl)), by = .(competencia, mun)]
d56 <- cg[divisao == "56", .(adm_56 = sum(adm), desl_56 = sum(desl)), by = .(competencia, mun)]

painel <- CJ(mun = unique(cg$mun), competencia = meses)
for (x in list(tot, d55, d56)) painel <- merge(painel, x, by = c("mun", "competencia"), all.x = TRUE)
num <- setdiff(names(painel), c("mun", "competencia"))
for (cc in num) painel[is.na(get(cc)), (cc) := 0]               # mes sem movimento = 0

painel[, `:=`(saldo = adm - desl, saldo_55 = adm_55 - desl_55, saldo_56 = adm_56 - desl_56)]

# saldo normalizado: % de um mes normal de admissoes do proprio municipio
ref <- painel[competencia >= as.Date(REF[1]) & competencia <= as.Date(REF[2]),
              .(adm_ref = mean(adm), adm_ref_55 = mean(adm_55), adm_ref_56 = mean(adm_56)), by = mun]
painel <- merge(painel, ref, by = "mun")
painel <- painel[adm_ref >= MIN_ADM]
painel[, `:=`(saldo_norm    = 100 * saldo / adm_ref,
              saldo_norm_55 = fifelse(adm_ref_55 > 0, 100 * saldo_55 / adm_ref_55, NA_real_),
              saldo_norm_56 = fifelse(adm_ref_56 > 0, 100 * saldo_56 / adm_ref_56, NA_real_))]

painel[, grupo := fifelse(mun == BELEM, "Belém", fifelse(mun %in% RM_RESTO, "RM (sem Belém)", "Interior"))]
painel[, `:=`(bel = as.integer(grupo == "Belém"), rm = as.integer(grupo == "RM (sem Belém)"),
              mes_ano = as.integer(format(competencia, "%m")),
              tempo   = format(competencia, "%Y-%m"))]
for (f in names(FASES)) {
  lim <- as.Date(FASES[[f]])
  painel[, (f) := as.integer(competencia >= lim[1] & competencia <= lim[2])]
  painel[, (paste0("Bel_", f)) := bel * get(f)]
  painel[, (paste0("RM_", f))  := rm  * get(f)]
}
painel[, fase := fifelse(prep == 1, "2 Preparação", fifelse(evento == 1, "3 Evento",
                 fifelse(desmob == 1, "4 Desmobilização", "1 Referência")))]
fwrite(painel, file.path(PROC, "painel_municipios.csv"))
message("municipios: ", uniqueN(painel$mun),
        " (", painel[, uniqueN(mun), by = grupo][, paste(grupo, V1, collapse = "; ")], ")")

# ---- controles macro (BCB) -------------------------------------------
# SGS 3698 cambio (venda, media do mes); 4390 Selic acumulada no mes; 433 IPCA mensal;
# 24364 IBC-Br dessazonalizado. Usa a atividade nacional porque o volume de servicos do
# Para e afetado pela propria COP (controle ruim).
sgs <- rbcb::get_series(c(cambio = 3698, selic = 4390, ipca = 433, ibc = 24364),
                        start_date = as.Date("2021-12-01"), end_date = FIM + 31)
macro <- Reduce(function(a, b) merge(a, b, by = "date"), lapply(sgs, as.data.table))
setnames(macro, "date", "competencia")
macro[, competencia := as.Date(format(competencia, "%Y-%m-01"))]
ivs <- fread(file.path(PROC, "base_cop30_para.csv"))[, .(competencia = as.Date(competencia), ln_ivs)]
macro <- merge(macro, ivs, by = "competencia", all.x = TRUE)
setorder(macro, competencia)
# controles em variacao, nao em nivel (Granger e Newbold, 1974)
macro[, `:=`(d_cambio = 100 * (log(cambio) - shift(log(cambio))),
             d_ivs    = 100 * (ln_ivs - shift(ln_ivs)),      # so descritivo
             d_ibc    = 100 * (log(ibc) - shift(log(ibc))),
             d_selic  = selic - shift(selic))]
# ipca ja e taxa mensal
fwrite(macro, file.path(PROC, "macro.csv"))

# ---- series por grupo ------------------------------------------------
serie <- painel[, .(saldo = sum(saldo), adm_ref = sum(adm_ref)), by = .(grupo, competencia, fase)]
rm_int <- painel[grupo != "Interior", .(saldo = sum(saldo), adm_ref = sum(adm_ref)), by = .(competencia, fase)]
rm_int[, grupo := "RM inteira"]                                   # Belem + vizinhos
serie <- rbind(serie, rm_int, use.names = TRUE)
serie[, saldo_norm := 100 * saldo / adm_ref]

# ---- base em Excel ---------------------------------------------------
nomes_mun <- c("150140" = "Belém", "150080" = "Ananindeua", "150442" = "Marituba",
               "150150" = "Benevides", "150635" = "Santa Bárbara do Pará",
               "150650" = "Santa Izabel do Pará", "150240" = "Castanhal")
xl_painel <- painel[, .(cod_ibge = mun,
                        municipio = fifelse(mun %in% names(nomes_mun), nomes_mun[mun], NA_character_),
                        grupo, mes = format(competencia, "%Y-%m"), fase,
                        admissoes = adm, desligamentos = desl, saldo,
                        saldo_55_alojamento = saldo_55, saldo_56_alimentacao = saldo_56,
                        adm_media_referencia = round(adm_ref, 2), saldo_norm = round(saldo_norm, 2))]
setorder(xl_painel, grupo, cod_ibge, mes)
xl_series <- merge(serie, macro, by = "competencia", all.x = TRUE)[
  , .(grupo, mes = format(competencia, "%Y-%m"), fase, saldo, adm_media_referencia = round(adm_ref, 2),
      saldo_norm = round(saldo_norm, 2), var_ibcbr = round(d_ibc, 3),
      var_cambio = round(d_cambio, 3), selic = selic, var_selic = round(d_selic, 4), ipca = ipca)]
setorder(xl_series, grupo, mes)
xl_macro <- macro[competencia >= INICIO, .(mes = format(competencia, "%Y-%m"), cambio, var_cambio = round(d_cambio, 3),
                                           selic, ipca, ln_volume_servicos = round(ln_ivs, 4),
                                           var_volume_servicos = round(d_ivs, 3))]
dicionario <- data.table(
  variavel = c("cod_ibge", "municipio", "grupo", "mes", "fase", "admissoes", "desligamentos", "saldo",
               "saldo_55_alojamento", "saldo_56_alimentacao", "adm_media_referencia", "saldo_norm",
               "cambio", "var_cambio", "selic", "ipca", "ln_volume_servicos", "var_volume_servicos"),
  descricao = c("Código IBGE do município (6 dígitos)",
                "Nome (preenchido para Belém e região metropolitana)",
                "Belém; RM (sem Belém); Interior; RM inteira = Belém + vizinhos (só na aba series_grupos)",
                "Mês de competência (ano-mês)",
                "Referência (jan/22-jun/24); Preparação (jul/24-out/25); Evento (nov/25); Desmobilização (dez/25-jul/26)",
                "Admissões formais em CNAE 55 + 56", "Desligamentos formais em CNAE 55 + 56",
                "Admissões - desligamentos", "Saldo só em Alojamento (CNAE 55)", "Saldo só em Alimentação (CNAE 56)",
                "Média mensal de admissões de jan/2022 a jun/2024",
                "100 x saldo / adm_media_referencia (% de um mês normal de admissões)",
                "Dólar comercial, venda, média do mês (R$/US$)", "Variação % do câmbio no mês",
                "Selic acumulada no mês (% a.m.)", "IPCA, variação no mês (%)",
                "Log do índice de volume de serviços do Pará", "Variação % do volume de serviços no mês"),
  fonte = c(rep("Novo CAGED (MTE): ftp://ftp.mtps.gov.br/pdet/microdados/NOVO%20CAGED/ - arquivos MOV + FOR - EXC, UF 15, subclasse CNAE 55 e 56, por município e mês de competência", 12),
            "BCB-SGS série 3698: https://www3.bcb.gov.br/sgspub", "BCB-SGS série 3698 (calculada)",
            "BCB-SGS série 4390", "BCB-SGS série 433",
            "IBGE/PMS tabela 5906: https://sidra.ibge.gov.br/tabela/5906", "IBGE/PMS tabela 5906 (calculada)"))
writexl::write_xlsx(list(painel_municipios = xl_painel, series_grupos = xl_series,
                         macro = xl_macro, dicionario = dicionario),
                    file.path(PROC, "base_helder.xlsx"))

# ---- tabela 1 e figura 1 ---------------------------------------------
tab1 <- dcast(serie[, .(media = round(mean(saldo_norm), 1)), by = .(grupo, fase)],
              grupo ~ fase, value.var = "media")
print(tab1)
datasummary_df(tab1, output = file.path(TAB, "tabela1_descritivas.tex"),
               title = "Saldo normalizado médio por grupo e fase (\\% de um mês normal de admissões)")

fases_df <- data.frame(fase = names(FASES),
                       ini = as.Date(sapply(FASES, `[`, 1)),
                       fim = as.Date(sapply(FASES, `[`, 2)) + 27)
g <- ggplot(serie, aes(competencia, saldo_norm, colour = grupo)) +
  geom_rect(data = fases_df, inherit.aes = FALSE,
            aes(xmin = ini, xmax = fim, ymin = -Inf, ymax = Inf, fill = fase), alpha = 0.12) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_line() +
  labs(x = NULL, y = "Saldo (% de um mês normal de admissões)", colour = "Grupo", fill = "Fase",
       caption = paste0("Fonte: Novo CAGED (MTE), ftp://ftp.mtps.gov.br/pdet/microdados/NOVO%20CAGED/ ",
                        "- arquivos MOV + FOR - EXC, UF 15 (Pará), CNAE 55 e 56,\nsomados por grupo de ",
                        "municípios e mês de competência; saldo dividido pela média de admissões de jan/2022 a jun/2024.")) +
  theme_minimal() + theme(legend.position = "bottom", plot.caption = element_text(hjust = 0, size = 7))
ggsave(file.path(FIG, "figura1_grupos.png"), g, width = 9, height = 5, dpi = 200)

# ---- modelo A: serie de cada grupo + controles macro -----------------
serieA <- merge(serie, macro, by = "competencia")
serieA[, `:=`(mes_f = factor(as.integer(format(competencia, "%m"))),
              t = as.integer(factor(competencia)))]
for (f in names(FASES)) {
  lim <- as.Date(FASES[[f]])
  serieA[, (f) := as.integer(competencia >= lim[1] & competencia <= lim[2])]
}
fA_sem <- saldo_norm ~ prep + evento + desmob + mes_f + t
fA_com <- saldo_norm ~ prep + evento + desmob + mes_f + t + d_ibc + d_cambio + d_selic + ipca
modA <- list()
for (gr in c("Belém", "RM (sem Belém)", "RM inteira", "Interior")) {
  modA[[paste(gr, "- sem macro")]] <- lm(fA_sem, data = serieA[grupo == gr])
  modA[[paste(gr, "- com macro")]] <- lm(fA_com, data = serieA[grupo == gr])
}
vc_nw <- function(m) sandwich::NeweyWest(m, prewhite = FALSE, adjust = TRUE)
nomesA <- c(prep = "Preparação", evento = "Evento (nov/2025)", desmob = "Desmobilização",
            d_ibc = "Var. % IBC-Br (atividade nacional)", d_cambio = "Var. % câmbio (R$/US$)",
            d_selic = "Var. Selic (p.p.)", ipca = "IPCA (% no mês)")
for (ext in c("html", "tex")) {
  modelsummary(modA, vcov = vc_nw, stars = c("*" = .1, "**" = .05, "***" = .01),
               coef_map = nomesA, gof_map = c("nobs", "r.squared"),
               title = "Modelo A (descritivo): saldo normalizado por grupo, MQO com dummies de mês e tendência",
               notes = c("Erros-padrão de Newey-West entre parênteses.",
                         "Sem grupo de comparação: os coeficientes descrevem antes e depois, não são efeito causal."),
               output = file.path(TAB, paste0("tabela2_modeloA.", ext)))
}

# ---- modelo B: painel (efeito local x transbordamento) ---------------
RHS_B <- "Bel_prep + Bel_evento + Bel_desmob + RM_prep + RM_evento + RM_desmob | mun^mes_ano + tempo"
fB    <- as.formula(paste("saldo_norm ~", RHS_B))
fB_55 <- as.formula(paste("saldo_norm_55 ~", RHS_B))
fB_56 <- as.formula(paste("saldo_norm_56 ~", RHS_B))
modB <- list(
  "(1) Principal"          = feols(fB, data = painel, cluster = ~mun),
  "(2) Só Alojamento (55)"  = feols(fB_55, data = painel, cluster = ~mun),
  "(3) Só Alimentação (56)" = feols(fB_56, data = painel, cluster = ~mun),
  "(4) RM sem Castanhal"   = feols(fB, data = painel[mun != "150240"], cluster = ~mun),
  "(5) Out-dez em bloco"   = feols(fB, data = copy(painel)[, `:=`(
                               Bel_prep   = bel * as.integer(competencia >= as.Date("2024-07-01") & competencia <= as.Date("2025-09-01")),
                               Bel_evento = bel * as.integer(competencia >= as.Date("2025-10-01") & competencia <= as.Date("2025-12-01")),
                               Bel_desmob = bel * as.integer(competencia >= as.Date("2026-01-01")),
                               RM_prep    = rm  * as.integer(competencia >= as.Date("2024-07-01") & competencia <= as.Date("2025-09-01")),
                               RM_evento  = rm  * as.integer(competencia >= as.Date("2025-10-01") & competencia <= as.Date("2025-12-01")),
                               RM_desmob  = rm  * as.integer(competencia >= as.Date("2026-01-01")))],
                               cluster = ~mun)
)
nomesB <- c(Bel_prep = "Belém x Preparação", Bel_evento = "Belém x Evento", Bel_desmob = "Belém x Desmobilização",
            RM_prep = "RM x Preparação", RM_evento = "RM x Evento", RM_desmob = "RM x Desmobilização")
for (ext in c("html", "tex")) {
  modelsummary(modB, stars = c("*" = .1, "**" = .05, "***" = .01),
               coef_map = nomesB, gof_map = c("nobs", "r.squared"),
               title = "Modelo B: efeito local (Belém) e transbordamento (RM), interior como controle",
               notes = "Efeitos fixos de município x mês do ano e de mês calendário. Erros agrupados por município.",
               output = file.path(TAB, paste0("tabela3_modeloB.", ext)))
}
print(etable(modB[["(1) Principal"]]))

# ---- placebo: cada municipio do interior no lugar de Belem -----------
# p_simples: fracao de municipios com efeito >= ao de Belem (em modulo)
# p_ponderado: idem, com cada efeito dividido pelo desvio-padrao do municipio na referencia
interior <- painel[grupo == "Interior", unique(mun)]
placebo <- function(y) {
  f <- as.formula(paste(y, "~", RHS_B))
  sdref <- painel[fase == "1 Referência", .(sd_ref = sd(get(y), na.rm = TRUE)), by = mun]
  est <- function(u) {
    d <- copy(painel[grupo != "Belém" | u == BELEM])
    d[, b := as.integer(mun == u)]
    d[, `:=`(Bel_prep = b * prep, Bel_evento = b * evento, Bel_desmob = b * desmob)]
    as.data.table(as.list(coef(feols(f, data = d, notes = FALSE))[c("Bel_prep", "Bel_evento", "Bel_desmob")]))[, mun := u]
  }
  pl <- merge(rbindlist(lapply(c(BELEM, interior), est)), sdref, by = "mun")
  bl <- pl[mun == BELEM]
  data.table(variavel = y, fase = c("Preparação", "Evento", "Desmobilização"),
             efeito = round(unlist(bl[, .(Bel_prep, Bel_evento, Bel_desmob)]), 2),
             p_simples = sapply(c("Bel_prep", "Bel_evento", "Bel_desmob"), function(v)
               round(mean(abs(pl[[v]]) >= abs(bl[[v]]), na.rm = TRUE), 3)),
             p_ponderado = sapply(c("Bel_prep", "Bel_evento", "Bel_desmob"), function(v)
               round(mean(abs(pl[[v]] / pl$sd_ref) >= abs(bl[[v]] / bl$sd_ref), na.rm = TRUE), 3)),
             municipios = nrow(pl))
}
plac_tab <- rbind(placebo("saldo_norm"), placebo("saldo_norm_55"), placebo("saldo_norm_56"))
fwrite(plac_tab, file.path(TAB, "placebo_belem.csv"))

# efeitos de Belem em vagas por mes (efeito em % x admissoes normais / 100)
ref_bel <- painel[mun == BELEM][1]
cA <- coef(modA[["Belém - com macro"]])
vagas <- data.table(
  modelo = c(rep("Modelo A (Belém, com macro)", 3), rep("Modelo B (total)", 3),
             rep("Modelo B (Alojamento)", 3), rep("Modelo B (Alimentação)", 3)),
  fase   = rep(c("Preparação", "Evento", "Desmobilização"), 4),
  efeito_pct = round(c(cA[c("prep", "evento", "desmob")],
                       plac_tab[variavel == "saldo_norm", efeito],
                       plac_tab[variavel == "saldo_norm_55", efeito],
                       plac_tab[variavel == "saldo_norm_56", efeito]), 2),
  admissoes_normais_mes = round(c(rep(ref_bel$adm_ref, 6), rep(ref_bel$adm_ref_55, 3),
                                  rep(ref_bel$adm_ref_56, 3)), 1))
vagas[, vagas_por_mes := round(efeito_pct * admissoes_normais_mes / 100, 1)]
print(vagas)
fwrite(vagas, file.path(TAB, "efeitos_em_vagas.csv"))

# ---- estudo de evento no Para (tendencias antes da COP) ---------------
# efeito mes a mes de Belem e de cada municipio do interior (placebo); base: 2022.
# A RM fica de fora (pode ter sido afetada pela COP).
dP <- copy(painel[grupo != "RM (sem Belém)"])
dP[, unidade := mun]
FONTE_PA <- paste0("Fonte: Novo CAGED (MTE), ftp://ftp.mtps.gov.br/pdet/microdados/NOVO%20CAGED/ - arquivos MOV + FOR - EXC, ",
                   "UF 15, CNAE 55 e 56, por município e mês de competência;\n",
                   "saldo dividido pela média de admissões de jan/2022 a jun/2024. Efeitos fixos de município x mês do ano ",
                   "e de mês calendário. Preto: Belém; cinza: cada município do interior como placebo; faixa: preparação.")
FONTE_PA_EV <- paste0(FONTE_PA, "\nEfeito de cada mês em relação a 2022.")
FONTE_PA_AC <- paste0(FONTE_PA, "\nSoma dos efeitos mensais desde jul/2024, em relação a jan/2022-jun/2024.")
setores <- c(saldo_norm = "Alojamento + Alimentação", saldo_norm_55 = "Alojamento (55)",
             saldo_norm_56 = "Alimentação (56)")
sufixo  <- c(saldo_norm = "", saldo_norm_55 = "_alojamento", saldo_norm_56 = "_alimentacao")

ev_res <- lapply(names(setores), function(y)
  estudo_evento(dP, y, BELEM, inicio = "2023-01-01", fim = FIM, inicio_pos = FASES$prep[1]))
ev_coef <- rbindlist(lapply(ev_res, `[[`, "coef"))
pretend <- rbindlist(lapply(ev_res, `[[`, "resumo"))
fwrite(ev_coef, file.path(PROC, "event_study_para.csv"))
fwrite(pretend, file.path(TAB, "pretendencia_para.csv"))
for (y in names(setores)) {
  grafico_placebo(ev_coef[variavel == y], BELEM, "efeito",
                  paste0("Efeito no saldo, ", setores[y], " (% de um mês normal)"), FONTE_PA_EV,
                  file.path(FIG, paste0("figura2_event_study_para", sufixo[y], ".png")))
}

# ---- efeito acumulado (o efeito ficou?) -------------------------------
# soma dos efeitos mensais desde jul/2024; base: jan/2022 a jun/2024
adm_bel <- c(saldo_norm = ref_bel$adm_ref, saldo_norm_55 = ref_bel$adm_ref_55,
             saldo_norm_56 = ref_bel$adm_ref_56)
DATAS_ACUM <- c("2025-10-01", "2025-11-01", "2025-12-01", "2026-07-01")
ac_res <- lapply(names(setores), function(y)
  efeito_acumulado(dP, y, BELEM, inicio_pos = FASES$prep[1], fim = FIM,
                   sd_ref = sd_referencia(dP, y, REF)))
ac_coef <- rbindlist(ac_res)
acum <- rbindlist(lapply(ac_res, function(a)
  tabela_acumulado(a, BELEM, DATAS_ACUM, adm_bel[a$variavel[1]])))
fwrite(ac_coef, file.path(PROC, "efeito_acumulado_para_mensal.csv"))
fwrite(acum, file.path(TAB, "efeito_acumulado_para.csv"))
for (y in names(setores)) {
  grafico_placebo(ac_coef[variavel == y], BELEM, "acum_pct",
                  paste0("Efeito acumulado, ", setores[y], " (% de um mês normal)"),
                  FONTE_PA_AC,
                  file.path(FIG, paste0("figura3_acumulado_para", sufixo[y], ".png")))
}

# ---- modelo C: precos (IPCA Belem - Brasil, SIDRA 7060) --------------
tryCatch({
  pega <- function(api) {
    x <- as.data.table(sidrar::get_sidra(api = api))
    col_item <- grep("item", names(x), ignore.case = TRUE, value = TRUE)[1]
    col_mes  <- grep("^Mês \\(Código\\)$|^M.s \\(C.digo\\)$", names(x), value = TRUE)[1]
    x <- x[grepl("Índice geral|Alimentação fora do domicílio|^[0-9.]*Hospedagem$", get(col_item))]
    x[, .(item = sub("^[0-9.]+", "", get(col_item)),
          competencia = as.Date(paste0(get(col_mes), "01"), "%Y%m%d"), valor = as.numeric(Valor))]
  }
  bel_p <- pega("/t/7060/n7/1501/v/63/p/all/c315/all")   # RM de Belem
  br_p  <- pega("/t/7060/n1/all/v/63/p/all/c315/all")    # Brasil
  precos <- merge(bel_p, br_p, by = c("item", "competencia"), suffixes = c("_belem", "_brasil"))
  precos <- precos[competencia >= INICIO & competencia <= FIM]
  precos[, dif := valor_belem - valor_brasil]              # p.p.
  precos[, mes_f := factor(as.integer(format(competencia, "%m")))]
  for (f in names(FASES)) {
    lim <- as.Date(FASES[[f]]); precos[, (f) := as.integer(competencia >= lim[1] & competencia <= lim[2])]
  }
  fwrite(precos, file.path(PROC, "precos_belem_brasil.csv"))
  modC <- lapply(split(precos, precos$item), function(d) lm(dif ~ prep + evento + desmob + mes_f, data = d))
  for (ext in c("html", "tex")) {
    modelsummary(modC, vcov = vc_nw, stars = c("*" = .1, "**" = .05, "***" = .01),
                 coef_map = c(prep = "Preparação", evento = "Evento (nov/2025)", desmob = "Desmobilização"),
                 gof_map = c("nobs", "r.squared"),
                 title = "Modelo C: inflação mensal de Belém menos a do Brasil (p.p.), por item do IPCA",
                 notes = "MQO com dummies de mês; erros de Newey-West entre parênteses.",
                 output = file.path(TAB, paste0("tabela4_precos.", ext)))
  }
  print(lapply(modC, function(m) round(coef(m)[c("prep", "evento", "desmob")], 3)))
}, error = function(e) message("modelo C nao rodou: ", conditionMessage(e)))

# ---- teste ADF (H0: raiz unitaria) -----------------------------------
b <- serieA[grupo == "Belém"][order(competencia)]
series_adf <- list(
  "Saldo normalizado - Belém"          = b$saldo_norm,
  "Saldo normalizado - RM sem Belém"   = serieA[grupo == "RM (sem Belém)"][order(competencia), saldo_norm],
  "Saldo normalizado - RM inteira"     = serieA[grupo == "RM inteira"][order(competencia), saldo_norm],
  "Saldo normalizado - Interior"       = serieA[grupo == "Interior"][order(competencia), saldo_norm],
  "Var. % IBC-Br"                      = b$d_ibc,
  "Var. % câmbio"                      = b$d_cambio,
  "Var. Selic (p.p.)"                  = b$d_selic,
  "Selic em nível (% no mês)"          = b$selic,
  "IPCA (% no mês)"                    = b$ipca)
adf <- rbindlist(lapply(names(series_adf), function(n) {
  t <- suppressWarnings(tseries::adf.test(na.omit(series_adf[[n]])))
  data.table(serie = n, estatistica_ADF = round(unname(t$statistic), 2), p_valor = round(t$p.value, 3))
}))
print(adf)
fwrite(adf, file.path(TAB, "teste_adf.csv"))

cat("\nplacebo (Belem x municipios do interior)\n"); print(plac_tab)
cat("\nefeitos de Belem em vagas por mes\n"); print(vagas)
cat("\ntendencias antes da COP (p_media_pre alto = bom; p_razao baixo = efeito se destaca)\n"); print(pretend)
cat("\nefeito acumulado de Belem\n"); print(acum)
cat("\nADF\n"); print(adf)

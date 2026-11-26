# 03_capitais.R
# Belem contra as outras capitais: diferencas em diferencas com placebo,
# testes de robustez, estudo de evento (tendencias antes da COP), efeito
# acumulado em vagas e controle sintetico. Total, alojamento (55) e alimentacao (56).
#
# rodar depois do 01_base_para.R (reaproveita os .7z de dados/brutos)
# saida: dados/processados/base_capitais.xlsx
#        resultados/tabelas/  capitais_did.csv, capitais_robustez.csv, capitais_pretendencia.csv,
#                             capitais_acumulado.csv, capitais_sc_pesos*.csv, capitais_sc_resultado*.csv
#        resultados/figuras/  capitais_event_study*.png, capitais_acumulado*.png,
#                             capitais_sc_trajetoria*.png

pacotes <- c("data.table", "archive", "fixest", "tidysynth", "dplyr", "ggplot2", "writexl")
novos <- setdiff(pacotes, rownames(installed.packages()))
if (length(novos)) install.packages(novos)
invisible(lapply(pacotes, library, character.only = TRUE))
options(timeout = 1800)

if (!file.exists("cop30.Rproj")) stop("abra o projeto cop30.Rproj antes de rodar")
source("R/funcoes.R")

BRUTOS <- "dados/brutos"
INTER  <- "dados/intermediarios/agregados_capitais"
PROC   <- "dados/processados"
TAB    <- "resultados/tabelas"
FIG    <- "resultados/figuras"
for (p in c(BRUTOS, INTER, PROC, TAB, FIG)) dir.create(p, recursive = TRUE, showWarnings = FALSE)

# parametros
INICIO   <- as.Date("2022-01-01")
FIM      <- as.Date("2026-07-01")
DIVISOES <- c("55", "56")
INICIO_ANALISE <- as.Date("2022-01-01")
REF <- as.Date(c("2022-01-01", "2024-06-01"))
TRATADA  <- "Belém"
EXCLUIR  <- c("Rio de Janeiro")                     # G20 em nov/2024
NORTE    <- c("Porto Velho", "Rio Branco", "Manaus", "Boa Vista", "Macapá", "Palmas")
FASES <- list(prep   = c("2024-07-01", "2025-10-01"),
              evento = c("2025-11-01", "2025-11-01"),
              desmob = c("2025-12-01", "2026-07-01"))

CAPITAIS <- c(
  "110020" = "Porto Velho",  "120040" = "Rio Branco",     "130260" = "Manaus",
  "140010" = "Boa Vista",    "150140" = "Belém",          "160030" = "Macapá",
  "172100" = "Palmas",       "211130" = "São Luís",       "221100" = "Teresina",
  "230440" = "Fortaleza",    "240810" = "Natal",          "250750" = "João Pessoa",
  "261160" = "Recife",       "270430" = "Maceió",         "280030" = "Aracaju",
  "292740" = "Salvador",     "310620" = "Belo Horizonte", "320530" = "Vitória",
  "330455" = "Rio de Janeiro", "355030" = "São Paulo",    "410690" = "Curitiba",
  "420540" = "Florianópolis", "431490" = "Porto Alegre",  "500270" = "Campo Grande",
  "510340" = "Cuiabá",       "520870" = "Goiânia",        "530010" = "Brasília")

# ---- CAGED por capital -----------------------------------------------
url_caged <- function(tipo, ano, mes) {
  sprintf("ftp://ftp.mtps.gov.br/pdet/microdados/NOVO%%20CAGED/%d/%d%02d/CAGED%s%d%02d.7z",
          ano, ano, mes, tipo, ano, mes)
}
achar_coluna <- function(nomes, padrao) {
  i <- grep(padrao, tolower(nomes), perl = TRUE)
  if (length(i) != 1) stop("coluna nao encontrada: ", padrao)
  nomes[i]
}
ler_caged_cap <- function(tipo, ano, mes) {
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
  dir_tmp <- file.path(tempdir(), sprintf("cap_%s%d%02d", tipo, ano, mes))
  archive::archive_extract(arq7z, dir = dir_tmp)
  txt <- list.files(dir_tmp, pattern = "\\.txt$", full.names = TRUE, ignore.case = TRUE, recursive = TRUE)[1]
  cab <- names(fread(txt, nrows = 0, sep = ";"))
  col <- c(competenciamov    = achar_coluna(cab, "^compet.*mov$"),
           municipio         = achar_coluna(cab, "^munic"),
           subclasse         = achar_coluna(cab, "^subclasse$"),
           saldomovimentacao = achar_coluna(cab, "^saldomovim"))
  dt <- fread(txt, sep = ";", select = unname(col), colClasses = "character", showProgress = FALSE)
  setnames(dt, unname(col), names(col))
  dt[, mun := substr(municipio, 1, 6)]
  dt <- dt[mun %in% names(CAPITAIS)]
  dt[, divisao := substr(sprintf("%07d", as.integer(subclasse)), 1, 2)]
  dt <- dt[divisao %in% DIVISOES]
  dt[, `:=`(mov = as.integer(saldomovimentacao),
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
  for (tipo in c("MOV", "FOR", "EXC")) lista[[paste(tipo, a, m)]] <- ler_caged_cap(tipo, a, m)
}
mov <- rbindlist(lista)
mov[, sinal := fifelse(tipo == "EXC", -1L, 1L)]
pn <- mov[competencia >= INICIO & competencia <= FIM,
          .(adm = sum(sinal * adm), desl = sum(sinal * desl)), by = .(competencia, mun, divisao)]

# ---- painel capital x mes --------------------------------------------
tot <- pn[, .(adm = sum(adm), desl = sum(desl)), by = .(competencia, mun)]
a55 <- pn[divisao == "55", .(adm_55 = sum(adm), desl_55 = sum(desl)), by = .(competencia, mun)]
a56 <- pn[divisao == "56", .(adm_56 = sum(adm), desl_56 = sum(desl)), by = .(competencia, mun)]
painel <- CJ(mun = names(CAPITAIS), competencia = meses)
for (x in list(tot, a55, a56)) painel <- merge(painel, x, by = c("mun", "competencia"), all.x = TRUE)
for (cc in c("adm", "desl", "adm_55", "desl_55", "adm_56", "desl_56")) painel[is.na(get(cc)), (cc) := 0]
painel[, cap := unname(CAPITAIS[mun])]
painel[, `:=`(saldo = adm - desl, saldo_55 = adm_55 - desl_55, saldo_56 = adm_56 - desl_56)]
ref <- painel[competencia >= REF[1] & competencia <= REF[2],
              .(adm_ref = mean(adm), adm_ref_55 = mean(adm_55), adm_ref_56 = mean(adm_56)), by = cap]
painel <- merge(painel, ref, by = "cap")
painel[, `:=`(saldo_norm    = 100 * saldo / adm_ref,
              saldo_norm_55 = fifelse(adm_ref_55 > 0, 100 * saldo_55 / adm_ref_55, NA_real_),
              saldo_norm_56 = fifelse(adm_ref_56 > 0, 100 * saldo_56 / adm_ref_56, NA_real_),
              share55       = fifelse(adm > 0, adm_55 / adm, NA_real_),
              mes_ano       = as.integer(format(competencia, "%m")),
              tempo         = format(competencia, "%Y-%m"))]
setorder(painel, cap, competencia)
# indice de estoque: saldo acumulado desde jan/2022, em % de um mes normal de admissoes
painel[competencia >= INICIO_ANALISE, `:=`(emp_idx    = 100 * cumsum(saldo) / adm_ref,
                                           emp_idx_55 = 100 * cumsum(saldo_55) / adm_ref_55), by = cap]
print(painel[, .(admissoes_total = sum(adm)), by = cap][order(admissoes_total)][1:5])

base <- painel[competencia >= INICIO_ANALISE & !cap %in% EXCLUIR]
writexl::write_xlsx(list(painel_capitais = base[, .(capital = cap, cod_ibge = mun, mes = tempo,
                                                    admissoes = adm, desligamentos = desl, saldo,
                                                    saldo_55_alojamento = saldo_55,
                                                    saldo_56_alimentacao = saldo_56,
                                                    adm_media_referencia = round(adm_ref, 2),
                                                    saldo_norm = round(saldo_norm, 2),
                                                    saldo_norm_55 = round(saldo_norm_55, 2),
                                                    saldo_norm_56 = round(saldo_norm_56, 2))]),
                    file.path(PROC, "base_capitais.xlsx"))

# ---- DiD + placebo ---------------------------------------------------
# efeito por fase para Belem e, como placebo, para cada outra capital
# p_simples: fracao de capitais com efeito >= ao de Belem (em modulo)
# p_ponderado: idem, dividindo pelo desvio-padrao da capital na referencia
estimar <- function(d, y = "saldo_norm", fases = FASES, excluir = character(0),
                    tirar_belem = NULL, ate = NULL) {
  d <- copy(d[!cap %in% excluir])
  if (!is.null(ate)) d <- d[competencia <= as.Date(ate)]
  if (!is.null(tirar_belem)) d <- d[!(cap == TRATADA & competencia %in% as.Date(tirar_belem))]
  d <- d[!is.na(get(y))]
  for (f in names(fases)) {
    lim <- as.Date(fases[[f]]); d[, (paste0("F_", f)) := as.integer(competencia >= lim[1] & competencia <= lim[2])]
  }
  fml <- as.formula(paste(y, "~", paste0("B_", names(fases), collapse = " + "), "| cap^mes_ano + tempo"))
  sd_ref <- d[competencia >= REF[1] & competencia <= REF[2], .(sd_ref = sd(get(y))), by = cap]
  uma <- function(u) {
    dd <- if (u == TRATADA) copy(d) else copy(d[cap != TRATADA])
    for (f in names(fases)) dd[, (paste0("B_", f)) := as.integer(cap == u) * get(paste0("F_", f))]
    m <- feols(fml, data = dd, warn = FALSE, notes = FALSE)
    as.data.table(as.list(coef(m)))[, cap := u]
  }
  res <- merge(rbindlist(lapply(unique(d$cap), uma), fill = TRUE), sd_ref, by = "cap")
  bel <- res[cap == TRATADA]
  data.table(
    fase = names(fases),
    efeito = sapply(names(fases), function(f) round(bel[[paste0("B_", f)]], 2)),
    p_simples = sapply(names(fases), function(f) { b <- paste0("B_", f)
    round(mean(abs(res[[b]]) >= abs(bel[[b]]), na.rm = TRUE), 3) }),
    p_ponderado = sapply(names(fases), function(f) { b <- paste0("B_", f)
    z <- res[[b]] / res$sd_ref; zb <- bel[[b]] / bel$sd_ref
    round(mean(abs(z) >= abs(zb), na.rm = TRUE), 3) }),
    capitais = nrow(res))
}
did <- rbind(estimar(base, "saldo_norm")[, setor := "Alojamento + Alimentação"],
             estimar(base, "saldo_norm_55")[, setor := "Só Alojamento (55)"],
             estimar(base, "saldo_norm_56")[, setor := "Só Alimentação (56)"])
print(did)
fwrite(did, file.path(TAB, "capitais_did.csv"))

# ---- robustez --------------------------------------------------------
specs <- list(
  "Janela curta (prep set-out/25)" = list(fases = list(prep = c("2025-09-01", "2025-10-01"),
                                                       evento = FASES$evento, desmob = FASES$desmob)),
  "Out-dez em bloco"              = list(fases = list(prep = c("2024-07-01", "2025-09-01"),
                                                      evento = c("2025-10-01", "2025-12-01"),
                                                      desmob = c("2026-01-01", "2026-07-01"))),
  "Sem capitais do Norte"         = list(excluir = NORTE),
  "Sem nov/2024 em Belém"         = list(tirar_belem = "2024-11-01"),
  "Placebo no tempo (-2 anos)"    = list(fases = list(prep = c("2022-07-01", "2023-10-01"),
                                                      evento = c("2023-11-01", "2023-11-01"),
                                                      desmob = c("2023-12-01", "2024-06-01")),
                                         ate = "2024-06-01"))
rob <- rbindlist(lapply(names(specs), function(nm) {
  s <- specs[[nm]]
  rbind(estimar(base, "saldo_norm", fases = if (is.null(s$fases)) FASES else s$fases,
                excluir = if (is.null(s$excluir)) character(0) else s$excluir,
                tirar_belem = s$tirar_belem, ate = s$ate)[, setor := "Total"],
        estimar(base, "saldo_norm_55", fases = if (is.null(s$fases)) FASES else s$fases,
                excluir = if (is.null(s$excluir)) character(0) else s$excluir,
                tirar_belem = s$tirar_belem, ate = s$ate)[, setor := "Alojamento"])[, teste := nm]
}))
print(rob)
fwrite(rob, file.path(TAB, "capitais_robustez.csv"))

# ---- estudo de evento (tendencias antes da COP) ----------------------
# efeito mes a mes de Belem e de cada capital (placebo) desde jan/2023; base: 2022
dC <- copy(base)
dC[, unidade := cap]
FONTE_CAP <- paste0("Fonte: Novo CAGED (MTE), ftp://ftp.mtps.gov.br/pdet/microdados/NOVO%20CAGED/ - arquivos MOV + FOR - EXC, ",
                    "CNAE 55 e 56, 26 capitais (exceto Rio de Janeiro),\nsomados por capital e mês de competência; ",
                    "saldo dividido pela média de admissões de jan/2022 a jun/2024. Efeitos fixos de capital x mês do ano ",
                    "e de mês calendário.\nPreto: Belém; cinza: cada capital como placebo; faixa: preparação.")
setores <- c(saldo_norm = "Alojamento + Alimentação", saldo_norm_55 = "Alojamento (55)",
             saldo_norm_56 = "Alimentação (56)")
sufixo  <- c(saldo_norm = "", saldo_norm_55 = "_alojamento", saldo_norm_56 = "_alimentacao")

ev_res <- lapply(names(setores), function(y)
  estudo_evento(dC, y, TRATADA, inicio = "2023-01-01", fim = FIM, inicio_pos = FASES$prep[1]))
ev_coef <- rbindlist(lapply(ev_res, `[[`, "coef"))
pretend <- rbindlist(lapply(ev_res, `[[`, "resumo"))
fwrite(ev_coef, file.path(PROC, "capitais_event_study.csv"))
fwrite(pretend, file.path(TAB, "capitais_pretendencia.csv"))
for (y in names(setores)) {
  grafico_placebo(ev_coef[variavel == y], TRATADA, "efeito",
                  paste0("Efeito no saldo, ", setores[y], " (% de um mês normal)"),
                  paste0(FONTE_CAP, " Efeito de cada mês em relação a 2022."),
                  file.path(FIG, paste0("capitais_event_study", sufixo[y], ".png")))
}

# ---- efeito acumulado (o efeito ficou?) -------------------------------
# soma dos efeitos mensais desde jul/2024; base: jan/2022 a jun/2024
ref_bel <- base[cap == TRATADA][1]
adm_bel <- c(saldo_norm = ref_bel$adm_ref, saldo_norm_55 = ref_bel$adm_ref_55,
             saldo_norm_56 = ref_bel$adm_ref_56)
DATAS_ACUM <- c("2025-10-01", "2025-11-01", "2025-12-01", "2026-07-01")
ac_res <- lapply(names(setores), function(y)
  efeito_acumulado(dC, y, TRATADA, inicio_pos = FASES$prep[1], fim = FIM,
                   sd_ref = sd_referencia(dC, y, REF)))
ac_coef <- rbindlist(ac_res)
acum <- rbindlist(lapply(ac_res, function(a)
  tabela_acumulado(a, TRATADA, DATAS_ACUM, adm_bel[a$variavel[1]])))
fwrite(ac_coef, file.path(PROC, "capitais_acumulado_mensal.csv"))
fwrite(acum, file.path(TAB, "capitais_acumulado.csv"))
for (y in names(setores)) {
  grafico_placebo(ac_coef[variavel == y], TRATADA, "acum_pct",
                  paste0("Efeito acumulado, ", setores[y], " (% de um mês normal)"),
                  paste0(FONTE_CAP, " Soma dos efeitos mensais desde jul/2024, em relação a jan/2022-jun/2024."),
                  file.path(FIG, paste0("capitais_acumulado", sufixo[y], ".png")))
}

# ---- controle sintetico ----------------------------------------------
# resultado: indice de estoque (saldo acumulado desde jan/2022, em % de um mes normal)
rodar_sc <- function(col_idx, col_saldo, sufixo) {
  sc_dt <- base[, .(cap, competencia, idx = get(col_idx), sn = get(col_saldo), share55)]
  sc_dt[, t := as.integer(factor(competencia))]
  T0 <- sc_dt[competencia < as.Date(FASES$prep[1]), max(t)]
  sc <- as.data.frame(sc_dt) |>
    synthetic_control(outcome = idx, unit = cap, time = t,
                      i_unit = TRATADA, i_time = T0, generate_placebos = TRUE) |>
    generate_predictor(time_window = 1:T0, saldo_medio = mean(sn, na.rm = TRUE),
                       share_aloj = mean(share55, na.rm = TRUE)) |>
    generate_predictor(time_window = 6,  idx_06 = idx) |>
    generate_predictor(time_window = 12, idx_12 = idx) |>
    generate_predictor(time_window = 18, idx_18 = idx) |>
    generate_predictor(time_window = T0, idx_T0 = idx) |>
    generate_weights(optimization_window = 1:T0) |>
    generate_control()
  fwrite(as.data.table(grab_unit_weights(sc))[order(-weight)],
         file.path(TAB, paste0("capitais_sc_pesos", sufixo, ".csv")))
  sig <- as.data.table(grab_significance(sc))[unit_name == TRATADA]
  serie <- as.data.table(grab_synthetic_control(sc))
  serie <- merge(serie, unique(sc_dt[, .(t, competencia)]), by.x = "time_unit", by.y = "t")
  serie[, gap := real_y - synth_y]                        # Belem menos Belem sintetica (estoque)
  fwrite(serie, file.path(PROC, paste0("capitais_sc_serie", sufixo, ".csv")))
  # figura: Belem observada x Belem sintetica (os pesos de cada capital estao em capitais_sc_pesos)
  g <- ggplot(melt(serie, id.vars = "competencia", measure.vars = c("real_y", "synth_y")),
              aes(competencia, value, linetype = variable)) +
    annotate("rect", xmin = as.Date(FASES$prep[1]), xmax = as.Date(FASES$evento[1]),
             ymin = -Inf, ymax = Inf, fill = "grey92") +
    geom_vline(xintercept = as.Date(FASES$evento[1]), linetype = 3) +
    geom_line() +
    scale_linetype_manual(values = c(real_y = 1, synth_y = 2),
                          labels = c(real_y = "Belém", synth_y = "Belém sintética")) +
    labs(x = NULL, y = "Saldo acumulado desde jan/2022 (% de um mês normal)", linetype = NULL,
         caption = paste0("Fonte: Novo CAGED (MTE), ftp://ftp.mtps.gov.br/pdet/microdados/NOVO%20CAGED/ - ",
                          "arquivos MOV + FOR - EXC, CNAE 55 e 56, 26 capitais (exceto Rio de Janeiro), somados por ",
                          "capital e mês de competência;\nsaldo acumulado desde jan/2022 dividido pela média de ",
                          "admissões de jan/2022 a jun/2024. Belém sintética: média ponderada das outras capitais, com ",
                          "pesos escolhidos\npara reproduzir Belém de jan/2022 a jun/2024 (pacote tidysynth). ",
                          "Faixa: preparação; linha pontilhada: nov/2025.")) +
    theme_minimal() + theme(legend.position = "bottom", plot.caption = element_text(hjust = 0, size = 7))
  ggsave(file.path(FIG, paste0("capitais_sc_trajetoria", sufixo, ".png")), g,
         width = 9, height = 4.8, dpi = 200)
  serie[, dgap := gap - shift(gap)]                       # diferenca mensal (fluxo)
  for (f in names(FASES)) {
    lim <- as.Date(FASES[[f]]); serie[competencia >= lim[1] & competencia <= lim[2], fase := f]
  }
  sc_res <- serie[!is.na(fase), .(dif_media_mensal = round(mean(dgap, na.rm = TRUE), 2),
                                  gap_fim_fase = round(gap[.N], 1)), by = fase]
  sc_res[, `:=`(mspe_ratio = round(sig$mspe_ratio, 2), rank = sig$rank,
                p_fisher = round(sig$fishers_exact_pvalue, 3))]
  print(sc_res)
  fwrite(sc_res, file.path(TAB, paste0("capitais_sc_resultado", sufixo, ".csv")))
  sc_res
}
sc_total <- tryCatch(rodar_sc("emp_idx", "saldo_norm", ""),
                     error = function(e) message("controle sintetico (total) nao rodou: ", conditionMessage(e)))
sc_aloj  <- tryCatch(rodar_sc("emp_idx_55", "saldo_norm_55", "_alojamento"),
                     error = function(e) message("controle sintetico (alojamento) nao rodou: ", conditionMessage(e)))

cat("\nDiD Belem x capitais (placebo)\n"); print(did)
cat("\ntendencias antes da COP (p_media_pre alto = bom; p_razao baixo = efeito se destaca)\n"); print(pretend)
cat("\nefeito acumulado de Belem\n"); print(acum)
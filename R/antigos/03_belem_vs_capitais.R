# =====================================================================
#  COP30 e emprego em Alojamento e Alimentação:
#  Belém comparada com as outras capitais (grupo de controle)
#
#  Estratégias (como em Hagn e Maennig, 2009, e Bronzini et al., 2019):
#    A) Diferenças em diferenças com efeitos fixos + inferência por placebo
#    B) Controle sintético (Abadie et al., 2010)
#
#  Usa os MESMOS arquivos .7z do CAGED já baixados pelo 02_base_completa.R
#  (pasta dados_caged/brutos). Só baixa o que estiver faltando.
#
#  Saídas (na pasta cop30):
#    painel_capitais.csv, did_resultados.csv, did_event_study.png,
#    sc_trajetorias.png, sc_diferenca.png, sc_placebos.png, sc_mspe.png,
#    sc_pesos.csv, sc_significancia.csv, sc_efeito_por_fase.csv
# =====================================================================

# ---- 0. Pacotes ------------------------------------------------------
pacotes <- c("data.table", "archive", "fixest", "tidysynth", "dplyr", "ggplot2")
novos <- setdiff(pacotes, rownames(installed.packages()))
if (length(novos)) install.packages(novos)
invisible(lapply(pacotes, library, character.only = TRUE))
options(timeout = 1800)

PASTA <- "C:/Users/User/Documents/cop30"
setwd(PASTA)

# ---- 1. Parâmetros ---------------------------------------------------
DIR_DADOS <- "dados_caged"
INICIO    <- as.Date("2021-01-01")   # meses lidos do CAGED
FIM       <- as.Date("2026-07-01")
DIVISOES  <- c("55", "56")

INICIO_ANALISE <- as.Date("2022-01-01")  # deixa de fora a recuperação da pandemia
REF_INI <- as.Date("2022-01-01")         # período "normal" usado para normalizar
REF_FIM <- as.Date("2024-06-01")

TRATADA <- "Belém"
# Doadoras a excluir por terem sediado outro grande evento no período
EXCLUIR_DOADORES <- c("Rio de Janeiro")  # cúpula do G20 em nov/2024

FASES <- list(
  D_prep   = c("2024-07-01", "2025-10-01"),
  D_evento = c("2025-11-01", "2025-11-01"),
  D_desmob = c("2025-12-01", "2026-07-01")
)
INICIO_TRAT_SC <- as.Date("2024-07-01")  # 1º mês tratado no controle sintético

# Capitais (código IBGE de 6 dígitos, como no CAGED). Confira se necessário.
CAPITAIS <- c(
  "110020" = "Porto Velho",  "120040" = "Rio Branco",     "130260" = "Manaus",
  "140010" = "Boa Vista",    "150140" = "Belém",          "160030" = "Macapá",
  "172100" = "Palmas",       "211130" = "São Luís",       "221100" = "Teresina",
  "230440" = "Fortaleza",    "240810" = "Natal",          "250750" = "João Pessoa",
  "261160" = "Recife",       "270430" = "Maceió",         "280030" = "Aracaju",
  "292740" = "Salvador",     "310620" = "Belo Horizonte", "320530" = "Vitória",
  "330455" = "Rio de Janeiro", "355030" = "São Paulo",    "410690" = "Curitiba",
  "420540" = "Florianópolis", "431490" = "Porto Alegre",  "500270" = "Campo Grande",
  "510340" = "Cuiabá",       "520870" = "Goiânia",        "530010" = "Brasília"
)

dir.create(file.path(DIR_DADOS, "brutos"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(DIR_DADOS, "agregados_capitais"), recursive = TRUE, showWarnings = FALSE)

# ---- 2. CAGED por capital --------------------------------------------
url_caged <- function(tipo, ano, mes) {
  sprintf("ftp://ftp.mtps.gov.br/pdet/microdados/NOVO%%20CAGED/%d/%d%02d/CAGED%s%d%02d.7z",
          ano, ano, mes, tipo, ano, mes)
}
achar_coluna <- function(nomes, padrao) {
  i <- grep(padrao, tolower(nomes), perl = TRUE)
  if (length(i) != 1) stop("Coluna não identificada com o padrão: ", padrao)
  nomes[i]
}

ler_caged_cap <- function(tipo, ano, mes) {
  cache <- file.path(DIR_DADOS, "agregados_capitais", sprintf("%s_%d%02d.rds", tipo, ano, mes))
  if (file.exists(cache)) return(readRDS(cache))

  arq7z <- file.path(DIR_DADOS, "brutos", sprintf("CAGED%s%d%02d.7z", tipo, ano, mes))
  if (!file.exists(arq7z)) {
    ok <- tryCatch({ download.file(url_caged(tipo, ano, mes), arq7z, mode = "wb", quiet = TRUE); TRUE },
                   error = function(e) FALSE)
    if (!ok || !file.exists(arq7z) || file.size(arq7z) < 1000) {
      unlink(arq7z); message("  indisponível: CAGED", tipo, ano, sprintf("%02d", mes)); return(NULL)
    }
  }

  dir_tmp <- file.path(tempdir(), sprintf("cap_%s%d%02d", tipo, ano, mes))
  archive::archive_extract(arq7z, dir = dir_tmp)
  txt <- list.files(dir_tmp, pattern = "\\.txt$", full.names = TRUE,
                    ignore.case = TRUE, recursive = TRUE)[1]

  cab <- names(fread(txt, nrows = 0, sep = ";"))
  col <- c(
    competenciamov    = achar_coluna(cab, "^compet.*mov$"),
    municipio         = achar_coluna(cab, "^munic"),
    subclasse         = achar_coluna(cab, "^subclasse$"),
    saldomovimentacao = achar_coluna(cab, "^saldomovim")
  )
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
message("\n===== ETAPA 1/3: CAGED por capital (", length(meses), " meses) =====")
lista <- list()
for (i in seq_along(meses)) {
  a <- as.integer(format(meses[i], "%Y")); m <- as.integer(format(meses[i], "%m"))
  message("CAGED ", format(meses[i], "%Y-%m"))
  for (tipo in c("MOV", "FOR", "EXC")) lista[[paste(tipo, a, m)]] <- ler_caged_cap(tipo, a, m)
}
mov <- rbindlist(lista)
mov[, sinal := fifelse(tipo == "EXC", -1L, 1L)]
pn <- mov[competencia >= INICIO & competencia <= FIM,
          .(adm = sum(sinal * adm), desl = sum(sinal * desl)), by = .(competencia, mun, divisao)]
meses_mov <- unique(mov[tipo == "MOV", competencia])

# ---- 3. Painel capitais x meses --------------------------------------
message("\n===== ETAPA 2/3: montando o painel =====")
tot <- pn[, .(adm = sum(adm), desl = sum(desl)), by = .(competencia, mun)]
a55 <- pn[divisao == "55", .(adm_55 = sum(adm)), by = .(competencia, mun)]

painel <- CJ(mun = names(CAPITAIS), competencia = meses)
painel <- merge(painel, tot, by = c("mun", "competencia"), all.x = TRUE)
painel <- merge(painel, a55, by = c("mun", "competencia"), all.x = TRUE)
painel <- painel[competencia %in% meses_mov]
for (cc in c("adm", "desl", "adm_55")) painel[is.na(get(cc)), (cc) := 0]
painel[, cap := unname(CAPITAIS[mun])]
painel[, saldo := adm - desl]

# Conferência: nenhuma capital pode ficar zerada (código errado)
conf <- painel[, .(admissoes_total = sum(adm)), by = cap][order(admissoes_total)]
print(conf)
if (any(conf$admissoes_total == 0)) warning("Há capital sem dados: confira o código IBGE em CAPITAIS.")

# Normalização: saldo como % das admissões de um mês "normal" de cada capital
ref <- painel[competencia >= REF_INI & competencia <= REF_FIM, .(adm_ref = mean(adm)), by = cap]
painel <- merge(painel, ref, by = "cap")
painel[, saldo_norm := 100 * saldo / adm_ref]
painel[, share55 := fifelse(adm > 0, adm_55 / adm, NA_real_)]
setorder(painel, cap, competencia)
painel[competencia >= INICIO_ANALISE, emp_idx := 100 * cumsum(saldo) / adm_ref, by = cap]

painel[, mes_ano := as.integer(format(competencia, "%m"))]
painel[, tratado := as.integer(cap == TRATADA)]
for (f in names(FASES)) {
  lim <- as.Date(FASES[[f]])
  painel[, (f) := as.integer(competencia >= lim[1] & competencia <= lim[2])]
}
fwrite(painel, "painel_capitais.csv")

# ---- 4A. Diferenças em diferenças + placebo --------------------------
message("\n===== ETAPA 3/3: estimações =====")
did <- painel[competencia >= INICIO_ANALISE & !cap %in% EXCLUIR_DOADORES]
meses_ev <- seq(as.Date(FASES$D_prep[1]), FIM, by = "month")
ev_cols  <- paste0("ev_", format(meses_ev, "%Y_%m"))
fml_ev   <- as.formula(paste("saldo_norm ~", paste(ev_cols, collapse = " + "),
                             "| cap^mes_ano + competencia"))

# Efeitos fixos: capital x mês do ano (sazonalidade local) e mês calendário
# (choques comuns a todas as capitais). Cada unidade é tratada uma vez (placebo).
est_unidade <- function(u) {
  d <- if (u == TRATADA) copy(did) else copy(did[cap != TRATADA])
  d[, tr := as.integer(cap == u)]
  d[, `:=`(B_prep = tr * D_prep, B_evento = tr * D_evento, B_desmob = tr * D_desmob)]
  for (k in seq_along(meses_ev)) d[, (ev_cols[k]) := as.integer(tr == 1L & competencia == meses_ev[k])]
  m1 <- feols(saldo_norm ~ B_prep + B_evento + B_desmob | cap^mes_ano + competencia, data = d)
  m2 <- feols(fml_ev, data = d)
  list(
    fases = data.table(cap = u, prep = coef(m1)[["B_prep"]], evento = coef(m1)[["B_evento"]],
                       desmob = coef(m1)[["B_desmob"]]),
    ev = data.table(cap = u, competencia = meses_ev, efeito = unname(coef(m2)[ev_cols]))
  )
}
res <- lapply(unique(did$cap), est_unidade)
fases_all <- rbindlist(lapply(res, `[[`, "fases"))
ev_all    <- rbindlist(lapply(res, `[[`, "ev"))

bel <- fases_all[cap == TRATADA]
adm_ref_bel <- ref[cap == TRATADA, adm_ref]
did_tab <- data.table(
  fase = c("Preparação", "Evento", "Desmobilização"),
  efeito_pct_adm_normal = c(bel$prep, bel$evento, bel$desmob),
  efeito_vagas_por_mes  = c(bel$prep, bel$evento, bel$desmob) * adm_ref_bel / 100,
  p_valor_placebo = sapply(c("prep", "evento", "desmob"),
                           function(v) mean(abs(fases_all[[v]]) >= abs(bel[[v]]))),
  n_unidades = nrow(fases_all)
)
print(did_tab)
fwrite(did_tab, "did_resultados.csv")

g_ev <- ggplot() +
  geom_line(data = ev_all[cap != TRATADA], aes(competencia, efeito, group = cap), color = "grey75") +
  geom_line(data = ev_all[cap == TRATADA], aes(competencia, efeito), color = "black", linewidth = 1) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_vline(xintercept = as.Date(FASES$D_evento[1]), color = "red") +
  labs(x = NULL, y = "Efeito no saldo (% das admissões de um mês normal)",
       title = "Belém (preto) e placebos das outras capitais (cinza)") +
  theme_minimal()
ggsave("did_event_study.png", g_ev, width = 9, height = 4.5, dpi = 150)

# ---- 4B. Controle sintético ------------------------------------------
sc_dt <- did[, .(cap, competencia, emp_idx, saldo_norm, share55)]
sc_dt[, t := as.integer(round(as.numeric(difftime(competencia, INICIO_ANALISE, units = "days")) / 30.44)) + 1L]
T0 <- sc_dt[competencia < INICIO_TRAT_SC, max(t)]   # último mês antes do tratamento

sc <- as.data.frame(sc_dt) |>
  synthetic_control(outcome = emp_idx, unit = cap, time = t,
                    i_unit = TRATADA, i_time = T0, generate_placebos = TRUE) |>
  generate_predictor(time_window = 1:T0,
                     saldo_medio = mean(saldo_norm, na.rm = TRUE),
                     share_aloj  = mean(share55, na.rm = TRUE)) |>
  generate_predictor(time_window = 6,  idx_06 = emp_idx) |>
  generate_predictor(time_window = 12, idx_12 = emp_idx) |>
  generate_predictor(time_window = 18, idx_18 = emp_idx) |>
  generate_predictor(time_window = 24, idx_24 = emp_idx) |>
  generate_predictor(time_window = T0, idx_T0 = emp_idx) |>
  generate_weights(optimization_window = 1:T0) |>
  generate_control()

ggsave("sc_trajetorias.png", plot_trends(sc),      width = 9, height = 4.5, dpi = 150)
ggsave("sc_diferenca.png",   plot_differences(sc), width = 9, height = 4.5, dpi = 150)
ggsave("sc_placebos.png",    plot_placebos(sc),    width = 9, height = 4.5, dpi = 150)
ggsave("sc_mspe.png",        plot_mspe_ratio(sc),  width = 9, height = 6,   dpi = 150)

pesos <- as.data.table(grab_unit_weights(sc))[order(-weight)]
print(pesos[weight > 0.01])
fwrite(pesos, "sc_pesos.csv")

sig <- as.data.table(grab_significance(sc))
print(sig[unit_name == TRATADA])
fwrite(sig, "sc_significancia.csv")

# Diferença Belém - sintético por fase, em vagas acumuladas
serie <- as.data.table(grab_synthetic_control(sc))
serie <- merge(serie, unique(sc_dt[, .(t, competencia)]), by.x = "time_unit", by.y = "t")
serie[, gap_vagas := (real_y - synth_y) * adm_ref_bel / 100]
for (f in names(FASES)) {
  lim <- as.Date(FASES[[f]])
  serie[competencia >= lim[1] & competencia <= lim[2], fase := f]
}
sc_fase <- serie[!is.na(fase), .(gap_medio_vagas = mean(gap_vagas)), by = fase]
print(sc_fase)
fwrite(sc_fase, "sc_efeito_por_fase.csv")

message("\nPronto. Abra did_resultados.csv, os gráficos .png e sc_significancia.csv.")

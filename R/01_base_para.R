# 01_base_para.R
# Serie mensal do Para (CNAE 55 + 56) a partir do Novo CAGED e indice de
# volume de servicos (PMS/IBGE). Tambem baixa os microdados do CAGED que os
# scripts 02 e 03 usam.
#
# entrada: microdados do Novo CAGED (baixados automaticamente para dados/brutos)
#          PMS/IBGE, tabela SIDRA 5906
# saida:   dados/processados/base_cop30_para.csv
#          resultados/tabelas/modelos_iniciais_para.html
#          resultados/figuras/saldo_para.png

pacotes <- c("data.table", "archive", "sidrar", "sandwich",
             "lmtest", "modelsummary", "ggplot2")
novos <- setdiff(pacotes, rownames(installed.packages()))
if (length(novos)) install.packages(novos)
invisible(lapply(pacotes, library, character.only = TRUE))
options(timeout = 1800)  # os .7z do CAGED sao grandes

if (!file.exists("cop30.Rproj")) stop("abra o projeto cop30.Rproj antes de rodar")

BRUTOS <- "dados/brutos"
INTER  <- "dados/intermediarios/agregados"
PROC   <- "dados/processados"
TAB    <- "resultados/tabelas"
FIG    <- "resultados/figuras"
for (p in c(BRUTOS, INTER, PROC, TAB, FIG)) dir.create(p, recursive = TRUE, showWarnings = FALSE)

# parametros
INICIO   <- as.Date("2021-01-01")
FIM      <- as.Date("2026-07-01")
UF_ALVO  <- 15L                # Para
DIVISOES <- c("55", "56")

# RM de Belem (codigo IBGE de 6 digitos, como no CAGED)
MUN_RMB <- c("150140",  # Belem
             "150080",  # Ananindeua
             "150442",  # Marituba
             "150150",  # Benevides
             "150635",  # Santa Barbara do Para
             "150650",  # Santa Izabel do Para
             "150240")  # Castanhal

FASES <- list(
  D_covid  = c("2021-01-01", "2021-12-01"),
  D_prep   = c("2024-07-01", "2025-10-01"),
  D_evento = c("2025-11-01", "2025-11-01"),
  D_desmob = c("2025-12-01", "2026-07-01")
)

# estoque de vinculos 55+56 no Para no fim do mes anterior a INICIO. NA = nao calcula taxa
ESTOQUE_INICIAL <- NA_real_

# serie do SIDRA (filtros em regex, minusculas)
SIDRA_TABELA  <- 5906   # a 8161 foi encerrada em dez/2022
SIDRA_INCLUIR <- c("volume")
SIDRA_EXCLUIR <- c("receita", "varia", "com ajuste", "acumulad")

# ---- CAGED -----------------------------------------------------------
url_caged <- function(tipo, ano, mes) {
  sprintf("ftp://ftp.mtps.gov.br/pdet/microdados/NOVO%%20CAGED/%d/%d%02d/CAGED%s%d%02d.7z",
          ano, ano, mes, tipo, ano, mes)
}

achar_coluna <- function(nomes, padrao) {
  i <- grep(padrao, tolower(nomes), perl = TRUE)
  if (length(i) != 1) stop("coluna nao encontrada: ", padrao)
  nomes[i]
}

ler_caged <- function(tipo, ano, mes) {
  cache <- file.path(INTER, sprintf("%s_%d%02d.rds", tipo, ano, mes))
  if (file.exists(cache)) return(readRDS(cache))

  arq7z <- file.path(BRUTOS, sprintf("CAGED%s%d%02d.7z", tipo, ano, mes))
  if (!file.exists(arq7z)) {
    ok <- tryCatch({
      download.file(url_caged(tipo, ano, mes), arq7z, mode = "wb", quiet = TRUE)
      TRUE
    }, error = function(e) FALSE)
    if (!ok || !file.exists(arq7z) || file.size(arq7z) < 1000) {
      unlink(arq7z)
      message("indisponivel: CAGED", tipo, ano, sprintf("%02d", mes))
      return(NULL)
    }
  }

  dir_tmp <- file.path(tempdir(), sprintf("%s%d%02d", tipo, ano, mes))
  archive::archive_extract(arq7z, dir = dir_tmp)
  txt <- list.files(dir_tmp, pattern = "\\.txt$", full.names = TRUE,
                    ignore.case = TRUE, recursive = TRUE)[1]

  cab <- names(fread(txt, nrows = 0, sep = ";"))
  col <- c(
    competenciamov    = achar_coluna(cab, "^compet.*mov$"),
    uf                = achar_coluna(cab, "^uf$"),
    municipio         = achar_coluna(cab, "^munic"),
    subclasse         = achar_coluna(cab, "^subclasse$"),
    saldomovimentacao = achar_coluna(cab, "^saldomovim")
  )

  dt <- fread(txt, sep = ";", select = unname(col),
              colClasses = "character", showProgress = FALSE)
  setnames(dt, unname(col), names(col))

  dt <- dt[as.integer(uf) == UF_ALVO]
  dt[, divisao := substr(sprintf("%07d", as.integer(subclasse)), 1, 2)]
  dt <- dt[divisao %in% DIVISOES]
  dt[, `:=`(
    mov         = as.integer(saldomovimentacao),
    rmb         = substr(municipio, 1, 6) %in% MUN_RMB,
    competencia = as.Date(paste0(competenciamov, "01"), "%Y%m%d")
  )]

  agg <- dt[, .(adm = sum(mov == 1L), desl = sum(mov == -1L)),
            by = .(competencia, divisao, rmb)]
  agg[, tipo := tipo]

  saveRDS(agg, cache)
  unlink(dir_tmp, recursive = TRUE)
  agg
}

meses <- seq(INICIO, FIM, by = "month")
lista <- list()
for (i in seq_along(meses)) {
  a <- as.integer(format(meses[i], "%Y"))
  m <- as.integer(format(meses[i], "%m"))
  message("caged ", format(meses[i], "%Y-%m"))
  for (tipo in c("MOV", "FOR", "EXC")) {
    lista[[paste(tipo, a, m)]] <- ler_caged(tipo, a, m)
  }
}

mov <- rbindlist(lista)
# FOR soma, EXC subtrai; tudo pela competencia do movimento
mov[, sinal := fifelse(tipo == "EXC", -1L, 1L)]
caged <- mov[competencia >= INICIO & competencia <= FIM,
             .(adm = sum(sinal * adm), desl = sum(sinal * desl)),
             by = .(competencia, divisao, rmb)]

cg_tot <- caged[, .(adm = sum(adm), desl = sum(desl)), by = competencia]
cg_div <- dcast(caged[, .(adm = sum(adm), desl = sum(desl)), by = .(competencia, divisao)],
                competencia ~ divisao, value.var = c("adm", "desl"))
cg_rmb <- caged[rmb == TRUE, .(adm_rmb = sum(adm), desl_rmb = sum(desl)), by = competencia]

# meses sem MOV ficam NA (nao zero)
meses_mov <- unique(mov[tipo == "MOV", competencia])

# ---- SIDRA / PMS -----------------------------------------------------
sid <- as.data.table(sidrar::get_sidra(x = SIDRA_TABELA, period = "all",
                                       geo = "State",
                                       geo.filter = list("State" = UF_ALVO)))

col_mes <- grep("^m[eê]s \\(c[oó]digo\\)$", names(sid), ignore.case = TRUE, value = TRUE)[1]
col_val <- grep("^valor$", names(sid), ignore.case = TRUE, value = TRUE)[1]
col_txt <- setdiff(names(sid)[sapply(sid, is.character)],
                   c(col_mes, grep("c[oó]digo|unidade da federa|^m[eê]s$",
                                   names(sid), ignore.case = TRUE, value = TRUE)))

sid[, desc := tolower(do.call(paste, c(.SD, sep = " | "))), .SDcols = col_txt]
filtro <- Reduce(`&`, lapply(SIDRA_INCLUIR, function(p) grepl(p, sid$desc))) &
          !Reduce(`|`, lapply(SIDRA_EXCLUIR, function(p) grepl(p, sid$desc)))
ivs <- sid[filtro & !is.na(get(col_val))]

if (anyDuplicated(ivs[[col_mes]])) {
  print(unique(ivs$desc))
  stop("mais de uma serie por mes depois do filtro; ajustar SIDRA_INCLUIR/SIDRA_EXCLUIR")
}
message("serie sidra: ", unique(ivs$desc))

ivs <- ivs[, .(competencia = as.Date(paste0(get(col_mes), "01"), "%Y%m%d"),
               ivs = as.numeric(get(col_val)))]

# ---- base ------------------------------------------------------------
base <- data.table(competencia = meses)
for (x in list(cg_tot, cg_div, cg_rmb, ivs)) base <- merge(base, x, by = "competencia", all.x = TRUE)

cols_caged <- setdiff(names(base), c("competencia", "ivs"))
base[!competencia %in% meses_mov, (cols_caged) := NA]
for (cc in cols_caged) base[competencia %in% meses_mov & is.na(get(cc)), (cc) := 0]

base[, `:=`(
  saldo     = adm - desl,
  saldo_55  = adm_55 - desl_55,
  saldo_56  = adm_56 - desl_56,
  saldo_rmb = adm_rmb - desl_rmb,
  ln_ivs    = log(ivs)
)]
base[, ln_ivs_l1 := shift(ln_ivs, 1)]
base[, estoque := ESTOQUE_INICIAL + shift(cumsum(fcoalesce(as.numeric(saldo), 0)), fill = 0)]
base[, taxa_saldo := 100 * saldo / estoque]

base[, `:=`(
  mes    = format(competencia, "%Y-%m"),
  ano    = as.integer(format(competencia, "%Y")),
  t      = seq_len(.N),
  mes_f  = factor(as.integer(format(competencia, "%m")), levels = c(12, 1:11))  # dez = ref.
)]
base[, D_2026 := as.integer(ano == 2026)]
for (f in names(FASES)) {
  lim <- as.Date(FASES[[f]])
  base[, (f) := as.integer(competencia >= lim[1] & competencia <= lim[2])]
}

# dummies mes a mes da desmobilizacao
meses_desmob <- base[D_desmob == 1, format(competencia, "%Y_%m")]
dm_cols <- paste0("Dm_", meses_desmob)
for (k in seq_along(dm_cols)) {
  base[, (dm_cols[k]) := as.integer(format(competencia, "%Y_%m") == meses_desmob[k])]
}

setcolorder(base, c("mes", "competencia", "ano", "t", "mes_f",
                    "adm", "desl", "saldo", "estoque", "taxa_saldo",
                    "adm_55", "desl_55", "saldo_55", "adm_56", "desl_56", "saldo_56",
                    "adm_rmb", "desl_rmb", "saldo_rmb", "ivs", "ln_ivs", "ln_ivs_l1"))
fwrite(base, file.path(PROC, "base_cop30_para.csv"))

# ---- grafico ---------------------------------------------------------
fases_df <- data.frame(
  fase = names(FASES),
  ini  = as.Date(sapply(FASES, `[`, 1)),
  fim  = as.Date(sapply(FASES, `[`, 2)) + 27
)
g <- ggplot(base, aes(competencia, saldo)) +
  geom_rect(data = fases_df, inherit.aes = FALSE,
            aes(xmin = ini, xmax = fim, ymin = -Inf, ymax = Inf, fill = fase), alpha = 0.15) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_line() + geom_point(size = 1) +
  labs(x = NULL, y = "Saldo mensal (adm - desl)", fill = "Fase",
       title = "Alojamento e Alimentação (CNAE 55+56) - Pará") +
  theme_minimal()
ggsave(file.path(FIG, "saldo_para.png"), g, width = 9, height = 4.5, dpi = 150)

# ---- modelos iniciais (serie do Para) --------------------------------
fases_ctrl <- "D_covid + D_prep + D_evento + D_desmob"
fm <- function(y, rhs) as.formula(paste(y, "~", rhs))

modelos <- list(
  "(1) Original"         = lm(fm("saldo", "ln_ivs + D_2026"), data = base),
  "(2) Fases + sazon."   = lm(fm("saldo", paste("ln_ivs +", fases_ctrl, "+ mes_f")), data = base),
  "(3) + tendência"      = lm(fm("saldo", paste("ln_ivs +", fases_ctrl, "+ mes_f + t")), data = base),
  "(4a) IVS defasado"    = lm(fm("saldo", paste("ln_ivs_l1 +", fases_ctrl, "+ mes_f + t")), data = base),
  "(4b) Sem IVS"         = lm(fm("saldo", paste(fases_ctrl, "+ mes_f + t")), data = base),
  "(5a) CNAE 55"         = lm(fm("saldo_55", paste("ln_ivs +", fases_ctrl, "+ mes_f + t")), data = base),
  "(5b) CNAE 56"         = lm(fm("saldo_56", paste("ln_ivs +", fases_ctrl, "+ mes_f + t")), data = base),
  "(5c) RM Belém"        = lm(fm("saldo_rmb", paste("ln_ivs +", fases_ctrl, "+ mes_f + t")), data = base)
)
if (!is.na(ESTOQUE_INICIAL)) {
  modelos[["(5d) Taxa (%)"]] <- lm(fm("taxa_saldo", paste("ln_ivs +", fases_ctrl, "+ mes_f + t")),
                                   data = base)
}
modelos[["(6) Mês a mês"]] <- lm(fm("saldo", paste("ln_ivs + D_covid + D_prep + D_evento +",
                                                   paste(dm_cols, collapse = " + "),
                                                   "+ mes_f + t")), data = base)

# Newey-West, banda automatica
vc_nw <- function(m) sandwich::NeweyWest(m, prewhite = FALSE, adjust = TRUE)

modelsummary(modelos, vcov = vc_nw, stars = c("*" = .1, "**" = .05, "***" = .01),
             coef_omit = "mes_f|Intercept", gof_map = c("nobs", "r.squared", "adj.r.squared"),
             notes = "Erros-padrão Newey-West. Dummies mensais incluídas quando indicado (dez = ref.).",
             output = file.path(TAB, "modelos_iniciais_para.html"))

for (nm in names(modelos)) {
  ct <- lmtest::coeftest(modelos[[nm]], vcov. = vc_nw(modelos[[nm]]))
  alvo <- intersect(c("D_2026", "D_desmob", dm_cols), rownames(ct))
  cat("\n", nm, "\n"); print(round(ct[alvo, , drop = FALSE], 3))
}

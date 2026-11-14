# =====================================================================
#  Emprego formal em Alojamento e Alimentação (CNAE 55 + 56) no Pará
#  e a desmobilização pós-COP30
#
#  Fontes:  Novo CAGED (microdados PDET/MTE: MOV + FOR - EXC)
#           PMS/IBGE via SIDRA (índice de volume de serviços, Pará)
#  Saídas:  base_cop30_para.csv, resultados_modelos.html, grafico_saldo.png
# =====================================================================

# ---- 0. Pacotes ------------------------------------------------------
pacotes <- c("data.table", "archive", "sidrar", "sandwich",
             "lmtest", "modelsummary", "ggplot2")
novos <- setdiff(pacotes, rownames(installed.packages()))
if (length(novos)) install.packages(novos)
invisible(lapply(pacotes, library, character.only = TRUE))

options(timeout = 1800)  # os .7z do CAGED são grandes

# Pasta de trabalho (a mesma do teste)
PASTA <- "C:/Users/User/Documents/cop30"
dir.create(PASTA, showWarnings = FALSE)
setwd(PASTA)
message("Pasta de trabalho: ", getwd())

# ---- 1. Parâmetros (ajuste aqui) -------------------------------------
DIR_DADOS <- "dados_caged"
INICIO    <- as.Date("2021-01-01")
FIM       <- as.Date("2026-07-01")      # último mês que você quer na base
UF_ALVO   <- 15L                        # Pará
DIVISOES  <- c("55", "56")

# Região Metropolitana de Belém (código IBGE de 6 dígitos, como no CAGED).
# Confira a composição vigente da RM antes de usar.
MUN_RMB <- c("150140",  # Belém
             "150080",  # Ananindeua
             "150442",  # Marituba
             "150150",  # Benevides
             "150635",  # Santa Bárbara do Pará
             "150650",  # Santa Izabel do Pará
             "150240")  # Castanhal

# Fases (datas = primeiro dia do mês). Mude para testar robustez.
FASES <- list(
  D_covid  = c("2021-01-01", "2021-12-01"),
  D_prep   = c("2024-07-01", "2025-10-01"),
  D_evento = c("2025-11-01", "2025-11-01"),
  D_desmob = c("2025-12-01", "2026-07-01")
)

# Estoque de vínculos CNAE 55+56 no Pará ao FINAL do mês anterior a INICIO
# (ex.: dez/2020). Pegue no painel/tabelas do Novo CAGED. NA = não calcula taxa.
ESTOQUE_INICIAL <- NA_real_

# Filtros para achar a série certa no SIDRA (regex, em minúsculas).
# Se o script parar dizendo que há mais de uma série por mês, refine aqui.
SIDRA_TABELA  <- 5906   # 8161 foi encerrada em dez/2022
SIDRA_INCLUIR <- c("volume")
SIDRA_EXCLUIR <- c("receita", "varia", "com ajuste", "acumulad")

dir.create(file.path(DIR_DADOS, "brutos"),     recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(DIR_DADOS, "agregados"),  recursive = TRUE, showWarnings = FALSE)

# ---- 2. Novo CAGED: download, leitura e agregação --------------------
url_caged <- function(tipo, ano, mes) {
  sprintf("ftp://ftp.mtps.gov.br/pdet/microdados/NOVO%%20CAGED/%d/%d%02d/CAGED%s%d%02d.7z",
          ano, ano, mes, tipo, ano, mes)
}

# Localiza colunas por padrão (robusto a acentos/encoding)
achar_coluna <- function(nomes, padrao) {
  i <- grep(padrao, tolower(nomes), perl = TRUE)
  if (length(i) != 1) stop("Coluna não identificada com o padrão: ", padrao)
  nomes[i]
}

ler_caged <- function(tipo, ano, mes) {
  cache <- file.path(DIR_DADOS, "agregados", sprintf("%s_%d%02d.rds", tipo, ano, mes))
  if (file.exists(cache)) return(readRDS(cache))

  arq7z <- file.path(DIR_DADOS, "brutos", sprintf("CAGED%s%d%02d.7z", tipo, ano, mes))
  if (!file.exists(arq7z)) {
    ok <- tryCatch({
      download.file(url_caged(tipo, ano, mes), arq7z, mode = "wb", quiet = TRUE)
      TRUE
    }, error = function(e) FALSE)
    if (!ok || !file.exists(arq7z) || file.size(arq7z) < 1000) {
      unlink(arq7z)
      message("  indisponível: CAGED", tipo, ano, sprintf("%02d", mes))
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
message("\n===== ETAPA 1/4: CAGED (", length(meses), " meses x 3 arquivos) =====")
lista <- list()
for (i in seq_along(meses)) {
  a <- as.integer(format(meses[i], "%Y"))
  m <- as.integer(format(meses[i], "%m"))
  message("CAGED ", format(meses[i], "%Y-%m"))
  for (tipo in c("MOV", "FOR", "EXC")) {
    lista[[paste(tipo, a, m)]] <- ler_caged(tipo, a, m)
  }
}

mov <- rbindlist(lista)
# FOR soma; EXC subtrai. Tudo atribuído pela competência do movimento.
mov[, sinal := fifelse(tipo == "EXC", -1L, 1L)]
caged <- mov[competencia >= INICIO & competencia <= FIM,
             .(adm = sum(sinal * adm), desl = sum(sinal * desl)),
             by = .(competencia, divisao, rmb)]

cg_tot <- caged[, .(adm = sum(adm), desl = sum(desl)), by = competencia]
cg_div <- dcast(caged[, .(adm = sum(adm), desl = sum(desl)), by = .(competencia, divisao)],
                competencia ~ divisao, value.var = c("adm", "desl"))
cg_rmb <- caged[rmb == TRUE, .(adm_rmb = sum(adm), desl_rmb = sum(desl)), by = competencia]

# Meses cujo MOV não foi baixado ficam NA (não zero)
meses_mov <- unique(mov[tipo == "MOV", competencia])

message("\n===== ETAPA 2/4: SIDRA =====")
# ---- 3. SIDRA / PMS --------------------------------------------------
# Rode sidrar::info_sidra(SIDRA_TABELA) se quiser ver a estrutura da tabela.
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
  stop("Mais de uma série por mês após o filtro. Ajuste SIDRA_INCLUIR/SIDRA_EXCLUIR.")
}
message("Série SIDRA usada: ", unique(ivs$desc))

ivs <- ivs[, .(competencia = as.Date(paste0(get(col_mes), "01"), "%Y%m%d"),
               ivs = as.numeric(get(col_val)))]

message("\n===== ETAPA 3/4: montando a base =====")
# ---- 4. Montagem da base ---------------------------------------------
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

# Dummies mês a mês da desmobilização (para ver a trajetória)
meses_desmob <- base[D_desmob == 1, format(competencia, "%Y_%m")]
dm_cols <- paste0("Dm_", meses_desmob)
for (k in seq_along(dm_cols)) {
  base[, (dm_cols[k]) := as.integer(format(competencia, "%Y_%m") == meses_desmob[k])]
}

setcolorder(base, c("mes", "competencia", "ano", "t", "mes_f",
                    "adm", "desl", "saldo", "estoque", "taxa_saldo",
                    "adm_55", "desl_55", "saldo_55", "adm_56", "desl_56", "saldo_56",
                    "adm_rmb", "desl_rmb", "saldo_rmb", "ivs", "ln_ivs", "ln_ivs_l1"))
fwrite(base, "base_cop30_para.csv")
message("Base salva: base_cop30_para.csv (", nrow(base), " meses)")

# ---- 5. Gráfico de controle ------------------------------------------
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
ggsave("grafico_saldo.png", g, width = 9, height = 4.5, dpi = 150)

message("\n===== ETAPA 4/4: modelos =====")
# ---- 6. Modelos ------------------------------------------------------
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

# Erros-padrão Newey-West (HAC), banda automática
vc_nw <- function(m) sandwich::NeweyWest(m, prewhite = FALSE, adjust = TRUE)

modelsummary(modelos, vcov = vc_nw, stars = c("*" = .1, "**" = .05, "***" = .01),
             coef_omit = "mes_f|Intercept", gof_map = c("nobs", "r.squared", "adj.r.squared"),
             notes = "Erros-padrão Newey-West. Dummies mensais incluídas quando indicado (dez = ref.).",
             output = "resultados_modelos.html")

# Resumo no console: coeficiente de interesse em cada modelo
for (nm in names(modelos)) {
  ct <- lmtest::coeftest(modelos[[nm]], vcov. = vc_nw(modelos[[nm]]))
  alvo <- intersect(c("D_2026", "D_desmob", dm_cols), rownames(ct))
  cat("\n==", nm, "==\n"); print(round(ct[alvo, , drop = FALSE], 3))
}
message("\nTabela completa: resultados_modelos.html | Gráfico: grafico_saldo.png")

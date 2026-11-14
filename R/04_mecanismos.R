# 04_mecanismos.R
# Mecanismos do efeito em Belem contra as outras capitais:
#   - tipo de vinculo: admissoes por prazo determinado, desligamentos por termino de
#     contrato e admissoes intermitentes (as vagas da COP foram temporarias?)
#   - salario de admissao (a COP pressionou salarios? canal de precos, Blanchard e Katz, 1992)
#
# rodar depois do 03_capitais.R (reaproveita os .7z de dados/brutos).
# Na primeira vez rele todos os .7z (demora); depois usa os agregados salvos.
#
# saida: dados/processados/painel_mecanismos.csv
#        resultados/tabelas/  tipos_movimentacao_encontrados.csv, mecanismos_descritivas_belem.csv,
#                             mecanismos_did.csv
#        resultados/figuras/  mecanismos_belem.png

pacotes <- c("data.table", "archive", "fixest", "ggplot2")
novos <- setdiff(pacotes, rownames(installed.packages()))
if (length(novos)) install.packages(novos)
invisible(lapply(pacotes, library, character.only = TRUE))
options(timeout = 1800)

if (!file.exists("cop30.Rproj")) stop("abra o projeto cop30.Rproj antes de rodar")
source("R/funcoes.R")

BRUTOS <- "dados/brutos"
INTER  <- "dados/intermediarios/agregados_mecanismos"
PROC   <- "dados/processados"
TAB    <- "resultados/tabelas"
FIG    <- "resultados/figuras"
for (p in c(BRUTOS, INTER, PROC, TAB, FIG)) dir.create(p, recursive = TRUE, showWarnings = FALSE)

# parametros
INICIO   <- as.Date("2022-01-01")
FIM      <- as.Date("2026-07-01")
DIVISOES <- c("55", "56")
REF      <- c("2022-01-01", "2024-06-01")
TRATADA  <- "Belém"
EXCLUIR  <- c("Rio de Janeiro")                     # G20 em nov/2024
FASES <- list(prep   = c("2024-07-01", "2025-10-01"),
              evento = c("2025-11-01", "2025-11-01"),
              desmob = c("2025-12-01", "2026-07-01"))

# codigos da variavel tipomovimentacao do Novo CAGED.
# Conferir no dicionario do MTE (Layout Novo Caged Movimentacao, na pasta NOVO CAGED do FTP)
# e na tabela resultados/tabelas/tipos_movimentacao_encontrados.csv que este script gera.
ADM_PRAZO    <- 25L          # admissao por contrato de trabalho por prazo determinado
FIM_CONTRATO <- c(43L, 45L)  # termino de contrato por prazo determinado; termino de contrato
SAL_MIN <- 300               # salarios fora desse intervalo sao tratados como erro de registro
SAL_MAX <- 30000
MIN_SAL_OBS <- 10            # minimo de admissoes com salario no mes para calcular a media

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

# ---- leitura do CAGED --------------------------------------------------
url_caged <- function(tipo, ano, mes) {
  sprintf("ftp://ftp.mtps.gov.br/pdet/microdados/NOVO%%20CAGED/%d/%d%02d/CAGED%s%d%02d.7z",
          ano, ano, mes, tipo, ano, mes)
}
achar_coluna <- function(nomes, padrao, obrigatoria = TRUE) {
  i <- grep(padrao, tolower(nomes), perl = TRUE)
  if (length(i) == 1) return(nomes[i])
  if (obrigatoria) stop("coluna nao encontrada: ", padrao)
  NA_character_
}
# salario vem como texto, as vezes com virgula decimal e ponto de milhar
num_br <- function(x) {
  x <- gsub("\\.(?=[0-9]{3}(,|$))", "", x, perl = TRUE)
  suppressWarnings(as.numeric(sub(",", ".", x, fixed = TRUE)))
}

ler_caged_mec <- function(tipo, ano, mes) {
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
  dir_tmp <- file.path(tempdir(), sprintf("mec_%s%d%02d", tipo, ano, mes))
  archive::archive_extract(arq7z, dir = dir_tmp)
  txt <- list.files(dir_tmp, pattern = "\\.txt$", full.names = TRUE, ignore.case = TRUE, recursive = TRUE)[1]
  cab <- names(fread(txt, nrows = 0, sep = ";"))
  col <- c(competenciamov    = achar_coluna(cab, "^compet.*mov$"),
           municipio         = achar_coluna(cab, "^munic"),
           subclasse         = achar_coluna(cab, "^subclasse$"),
           saldomovimentacao = achar_coluna(cab, "^saldomovim"),
           tipomovimentacao  = achar_coluna(cab, "^tipomovimenta"),
           salario           = achar_coluna(cab, "^sal.{1,2}rio$"),
           intermitente      = achar_coluna(cab, "^indtrabintermit", obrigatoria = FALSE))
  col <- col[!is.na(col)]
  dt <- fread(txt, sep = ";", select = unname(col), colClasses = "character", showProgress = FALSE)
  setnames(dt, unname(col), names(col))
  dt[, mun := substr(municipio, 1, 6)]
  dt <- dt[mun %in% names(CAPITAIS)]
  dt[, divisao := substr(sprintf("%07d", as.integer(subclasse)), 1, 2)]
  dt <- dt[divisao %in% DIVISOES]
  if (!"intermitente" %in% names(dt)) dt[, intermitente := NA_character_]
  dt[, `:=`(mov         = as.integer(saldomovimentacao),
            tipomov     = as.integer(tipomovimentacao),
            interm      = as.integer(tolower(intermitente) %in% c("1", "sim")),
            sal         = num_br(salario),
            competencia = as.Date(paste0(competenciamov, "01"), "%Y%m%d"))]
  dt[, lnsal := fifelse(mov == 1L & sal > SAL_MIN & sal < SAL_MAX, log(pmax(sal, 1)), NA_real_)]
  agg <- dt[, .(n = .N, soma_lnsal = sum(lnsal, na.rm = TRUE), n_sal = sum(!is.na(lnsal))),
            by = .(competencia, mun, divisao, mov, tipomov, interm)]
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
  for (tipo in c("MOV", "FOR", "EXC")) lista[[paste(tipo, a, m)]] <- ler_caged_mec(tipo, a, m)
}
mov <- rbindlist(lista)
mov[, sinal := fifelse(tipo == "EXC", -1L, 1L)]
cg <- mov[competencia >= INICIO & competencia <= FIM,
          .(n = sum(sinal * n), soma_lnsal = sum(sinal * soma_lnsal), n_sal = sum(sinal * n_sal)),
          by = .(competencia, mun, divisao, mov, tipomov, interm)]

# codigos encontrados, para conferir com o dicionario do MTE
tipos <- cg[, .(registros = sum(n)), by = .(movimento = fifelse(mov == 1L, "admissao", "desligamento"), tipomov)]
setorder(tipos, movimento, tipomov)
print(tipos)
fwrite(tipos, file.path(TAB, "tipos_movimentacao_encontrados.csv"))
tem_interm <- cg[mov == 1L & interm == 1L, sum(n)] > 0
if (!tem_interm) message("sem informacao de trabalho intermitente; esse resultado fica de fora")

# ---- painel capital x mes x setor -------------------------------------
agrega <- function(x) x[, .(adm        = sum(n[mov == 1L]),
                            desl       = sum(n[mov == -1L]),
                            adm_prazo  = sum(n[mov == 1L & tipomov %in% ADM_PRAZO]),
                            fim_contr  = sum(n[mov == -1L & tipomov %in% FIM_CONTRATO]),
                            adm_interm = sum(n[mov == 1L & interm == 1L]),
                            soma_lnsal = sum(soma_lnsal[mov == 1L]),
                            n_sal      = sum(n_sal[mov == 1L])), by = .(competencia, mun)]
setores_dt <- rbind(agrega(cg)[, setor := "Alojamento + Alimentação"],
                    agrega(cg[divisao == "55"])[, setor := "Alojamento (55)"],
                    agrega(cg[divisao == "56"])[, setor := "Alimentação (56)"])

pm <- CJ(mun = names(CAPITAIS), competencia = meses, setor = unique(setores_dt$setor))
pm <- merge(pm, setores_dt, by = c("mun", "competencia", "setor"), all.x = TRUE)
num <- c("adm", "desl", "adm_prazo", "fim_contr", "adm_interm", "soma_lnsal", "n_sal")
for (cc in num) pm[is.na(get(cc)), (cc) := 0]
pm[, cap := unname(CAPITAIS[mun])]
pm <- pm[!cap %in% EXCLUIR]
ref <- pm[competencia >= as.Date(REF[1]) & competencia <= as.Date(REF[2]),
          .(adm_ref = mean(adm)), by = .(cap, setor)]
pm <- merge(pm, ref, by = c("cap", "setor"))
pm[, `:=`(adm_prazo_norm   = 100 * adm_prazo / adm_ref,
          adm_outras_norm  = 100 * (adm - adm_prazo) / adm_ref,
          fim_contr_norm   = 100 * fim_contr / adm_ref,
          desl_outros_norm = 100 * (desl - fim_contr) / adm_ref,
          adm_interm_norm  = 100 * adm_interm / adm_ref,
          ln_sal           = fifelse(n_sal >= MIN_SAL_OBS, soma_lnsal / n_sal, NA_real_),
          unidade          = cap,
          mes_ano          = as.integer(format(competencia, "%m")),
          tempo            = format(competencia, "%Y-%m"))]
pm[, fase := "referencia"]
for (f in names(FASES)) {
  lim <- as.Date(FASES[[f]]); pm[competencia >= lim[1] & competencia <= lim[2], fase := f]
}
setorder(pm, setor, cap, competencia)
fwrite(pm, file.path(PROC, "painel_mecanismos.csv"))

# ---- descritivas de Belem por fase ------------------------------------
desc <- pm[cap == TRATADA,
           .(admissoes         = sum(adm),
             pct_adm_prazo      = round(100 * sum(adm_prazo) / sum(adm), 1),
             pct_desl_fim_contr = round(100 * sum(fim_contr) / sum(desl), 1),
             pct_adm_interm     = round(100 * sum(adm_interm) / sum(adm), 1),
             salario_medio_adm  = round(exp(sum(soma_lnsal) / sum(n_sal)))),
           by = .(setor, fase)]
print(desc)
fwrite(desc, file.path(TAB, "mecanismos_descritivas_belem.csv"))

# ---- DiD por fase com placebo -----------------------------------------
# ln_sal: o efeito x 100 e aproximadamente a variacao % do salario de admissao
Y <- c(adm_prazo_norm   = "Admissões por prazo determinado",
       adm_outras_norm  = "Demais admissões",
       fim_contr_norm   = "Desligamentos por término de contrato",
       desl_outros_norm = "Demais desligamentos",
       adm_interm_norm  = "Admissões intermitentes",
       ln_sal           = "Log do salário de admissão")
if (!tem_interm) Y <- Y[names(Y) != "adm_interm_norm"]

mec <- rbindlist(lapply(unique(pm$setor), function(st) {
  d <- pm[setor == st]
  rbindlist(lapply(names(Y), function(y)
    did_fases(d, y, TRATADA, FASES, sd_referencia(d, y, REF))[, `:=`(setor = st, resultado = Y[y])]))
}))
setcolorder(mec, c("setor", "resultado"))
print(mec)
fwrite(mec, file.path(TAB, "mecanismos_did.csv"))

# ---- figura: admissoes e desligamentos de Belem por tipo --------------
fig <- melt(pm[cap == TRATADA & setor == "Alojamento (55)",
               .(competencia, `Admissões por prazo determinado` = adm_prazo,
                 `Demais admissões` = adm - adm_prazo,
                 `Desligamentos por término de contrato` = fim_contr,
                 `Demais desligamentos` = desl - fim_contr)],
            id.vars = "competencia", variable.name = "tipo", value.name = "vinculos")
g <- ggplot(fig, aes(competencia, vinculos, linetype = tipo, colour = tipo)) +
  annotate("rect", xmin = as.Date(FASES$prep[1]), xmax = as.Date(FASES$evento[1]),
           ymin = -Inf, ymax = Inf, fill = "grey92") +
  geom_line() +
  geom_vline(xintercept = as.Date(FASES$evento[1]), linetype = 3) +
  scale_colour_manual(values = c("black", "black", "grey55", "grey55")) +
  scale_linetype_manual(values = c(1, 2, 1, 2)) +
  labs(x = NULL, y = "Vínculos no mês", colour = NULL, linetype = NULL,
       caption = paste0("Fonte: Novo CAGED (MTE), ftp://ftp.mtps.gov.br/pdet/microdados/NOVO%20CAGED/ - arquivos MOV + FOR - EXC, ",
                        "município 150140 (Belém), CNAE 55,\npor mês de competência e tipo de movimentação ",
                        "(prazo determinado: código ", ADM_PRAZO, "; término de contrato: códigos ",
                        paste(FIM_CONTRATO, collapse = " e "), "). Faixa: preparação.")) +
  theme_minimal() + theme(legend.position = "bottom", plot.caption = element_text(hjust = 0, size = 7))
ggsave(file.path(FIG, "mecanismos_belem.png"), g, width = 9, height = 5, dpi = 200)

cat("\nBelem por fase\n"); print(desc)
cat("\nDiD Belem x capitais, por tipo de vinculo e salario (placebo)\n"); print(mec)

# =====================================================================
#  ENTREGA DO HELDER (Cap. 5): efeito local e transbordamento da COP30
#  no emprego formal em Alojamento e Alimentação (CNAE 55 + 56) no Pará
#
#  ANTES DE RODAR: o 02_base_completa.R já precisa ter rodado uma vez.
#  Este script reaproveita os arquivos que o 02 deixou na pasta cop30:
#     - dados_caged/brutos/*.7z  (microdados do CAGED já baixados)
#     - base_cop30_para.csv      (índice de volume de serviços do Pará)
#
#  COMO RODAR: abra no RStudio e clique em "Source" (canto superior direito).
#  Na primeira vez demora (30 a 60 min), porque relê os arquivos do CAGED.
#  Da segunda vez em diante, leva 1 ou 2 minutos.
#
#  SAÍDAS (todas na mesma pasta onde este script está salvo, ex.: Documentos > helder):
#     base_helder.xlsx          BASE LIMPA NO EXCEL (painel, séries, macro e dicionário)
#     painel_municipios.csv     base município x mês
#     tabela1_descritivas.tex   médias por grupo e fase
#     tabela2_modeloA.tex/.html Modelo A: série de cada grupo + controles macro
#     tabela3_modeloB.tex/.html Modelo B: painel de municípios (local x transbordamento)
#     placebo_belem.csv         teste de placebo do efeito de Belém
#     figura1_grupos.png        saldo normalizado por grupo ao longo do tempo
# =====================================================================

# ---- PASSO 0. Pacotes ------------------------------------------------
message("PASSO 0/7 - Carregando pacotes...")
pacotes <- c(
  "data.table",
  "archive",
  "fixest",
  "rbcb",
  "sandwich",
  "writexl",
  "lmtest",
  "modelsummary",
  "ggplot2"
)
novos <- setdiff(pacotes, rownames(installed.packages()))
if (length(novos))
  install.packages(novos)
invisible(lapply(pacotes, library, character.only = TRUE))
options(timeout = 1800)

# Onde ficam as coisas:
#   SAIDA = a pasta onde este script está salvo (Documentos > helder): os resultados vão para lá
#   DADOS = a pasta cop30 (ao lado, dentro de Documentos): de lá vêm os dados do CAGED e do 02
SAIDA <- tryCatch(
  dirname(rstudioapi::getSourceEditorContext()$path),
  error = function(e)
    ""
)
if (!nzchar(SAIDA) ||
    SAIDA == ".")
  SAIDA <- tryCatch(
    dirname(sys.frame(1)$ofile),
    error = function(e)
      getwd()
  )
message("  Resultados vão para: ", SAIDA)

candidatas <- file.path(dirname(SAIDA), c("cop30", "cop 30"))
DADOS <- candidatas[file.exists(file.path(candidatas, "base_cop30_para.csv"))][1]
if (is.na(DADOS)) {
  message(">>> Escolha a pasta cop30 (a que tem o base_cop30_para.csv) na janela que vai abrir.")
  message(
    ">>> Se a janela não aparecer, procure na barra de tarefas: ela pode estar atrás do RStudio."
  )
  DADOS <- choose.dir(caption = "Escolha a pasta cop30")
}
setwd(DADOS)
message("  Dados vêm de: ", getwd())
dir.create(
  file.path("dados_caged", "agregados_mun"),
  recursive = TRUE,
  showWarnings = FALSE
)
dir.create(file.path("dados_caged", "brutos"),
           recursive = TRUE,
           showWarnings = FALSE)

if (!file.exists("base_cop30_para.csv"))
  stop("Não achei base_cop30_para.csv. Rode o 02_base_completa.R antes deste script.")

# ---- PASSO 1. Parâmetros ---------------------------------------------
INICIO   <- as.Date("2022-01-01")
FIM      <- as.Date("2026-07-01")
UF_ALVO  <- 15L
DIVISOES <- c("55", "56")
MIN_ADM  <- 5          # municípios com menos de 5 admissões/mês na referência saem

BELEM   <- "150140"
RM_RESTO <- c(
  "150080",
  # Ananindeua
  "150442",
  # Marituba
  "150150",
  # Benevides
  "150635",
  # Santa Bárbara do Pará
  "150650",
  # Santa Izabel do Pará
  "150240"
)  # Castanhal

REF   <- c("2022-01-01", "2024-06-01")   # período de referência
FASES <- list(
  prep   = c("2024-07-01", "2025-10-01"),
  evento = c("2025-11-01", "2025-11-01"),
  desmob = c("2025-12-01", "2026-07-01")
)

# ---- PASSO 2. CAGED por município ------------------------------------
message("PASSO 1/7 - Lendo o CAGED por município (na 1a vez demora)...")
url_caged <- function(tipo, ano, mes) {
  sprintf(
    "ftp://ftp.mtps.gov.br/pdet/microdados/NOVO%%20CAGED/%d/%d%02d/CAGED%s%d%02d.7z",
    ano,
    ano,
    mes,
    tipo,
    ano,
    mes
  )
}
achar_coluna <- function(nomes, padrao) {
  i <- grep(padrao, tolower(nomes), perl = TRUE)
  if (length(i) != 1)
    stop("Coluna não identificada com o padrão: ", padrao)
  nomes[i]
}

ler_caged_mun <- function(tipo, ano, mes) {
  cache <- file.path("dados_caged",
                     "agregados_mun",
                     sprintf("%s_%d%02d.rds", tipo, ano, mes))
  if (file.exists(cache))
    return(readRDS(cache))
  
  arq7z <- file.path("dados_caged",
                     "brutos",
                     sprintf("CAGED%s%d%02d.7z", tipo, ano, mes))
  if (!file.exists(arq7z)) {
    ok <- tryCatch({
      download.file(url_caged(tipo, ano, mes),
                    arq7z,
                    mode = "wb",
                    quiet = TRUE)
      TRUE
    }, error = function(e)
      FALSE)
    if (!ok || !file.exists(arq7z) || file.size(arq7z) < 1000) {
      unlink(arq7z)
      message("  indisponível: CAGED", tipo, ano, sprintf("%02d", mes))
      return(NULL)
    }
  }
  dir_tmp <- file.path(tempdir(), sprintf("mun_%s%d%02d", tipo, ano, mes))
  archive::archive_extract(arq7z, dir = dir_tmp)
  txt <- list.files(
    dir_tmp,
    pattern = "\\.txt$",
    full.names = TRUE,
    ignore.case = TRUE,
    recursive = TRUE
  )[1]
  cab <- names(fread(txt, nrows = 0, sep = ";"))
  col <- c(
    competenciamov    = achar_coluna(cab, "^compet.*mov$"),
    uf                = achar_coluna(cab, "^uf$"),
    municipio         = achar_coluna(cab, "^munic"),
    subclasse         = achar_coluna(cab, "^subclasse$"),
    saldomovimentacao = achar_coluna(cab, "^saldomovim")
  )
  dt <- fread(
    txt,
    sep = ";",
    select = unname(col),
    colClasses = "character",
    showProgress = FALSE
  )
  setnames(dt, unname(col), names(col))
  dt <- dt[as.integer(uf) == UF_ALVO]
  dt[, divisao := substr(sprintf("%07d", as.integer(subclasse)), 1, 2)]
  dt <- dt[divisao %in% DIVISOES]
  dt[, `:=`(
    mov = as.integer(saldomovimentacao),
    mun = substr(municipio, 1, 6),
    competencia = as.Date(paste0(competenciamov, "01"), "%Y%m%d")
  )]
  agg <- dt[, .(adm = sum(mov == 1L), desl = sum(mov == -1L)), by = .(competencia, mun, divisao)]
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
  message("  CAGED ", format(meses[i], "%Y-%m"))
  for (tipo in c("MOV", "FOR", "EXC"))
    lista[[paste(tipo, a, m)]] <- ler_caged_mun(tipo, a, m)
}
mov <- rbindlist(lista)
mov[, sinal := fifelse(tipo == "EXC", -1L, 1L)]
cg <- mov[competencia >= INICIO & competencia <= FIM, .(adm = sum(sinal * adm), desl = sum(sinal * desl)), by = .(competencia, mun, divisao)]

# ---- PASSO 3. Painel balanceado município x mês ----------------------
message("PASSO 2/7 - Montando o painel de municípios...")
tot <- cg[, .(adm = sum(adm), desl = sum(desl)), by = .(competencia, mun)]
d55 <- cg[divisao == "55", .(adm_55 = sum(adm), desl_55 = sum(desl)), by = .(competencia, mun)]
d56 <- cg[divisao == "56", .(adm_56 = sum(adm), desl_56 = sum(desl)), by = .(competencia, mun)]

painel <- CJ(mun = unique(cg$mun), competencia = meses)        # todos os municípios em todos os meses
for (x in list(tot, d55, d56))
  painel <- merge(painel,
                  x,
                  by = c("mun", "competencia"),
                  all.x = TRUE)
num <- setdiff(names(painel), c("mun", "competencia"))
for (cc in num)
  painel[is.na(get(cc)), (cc) := 0]               # mês sem movimento = 0

painel[, `:=`(
  saldo = adm - desl,
  saldo_55 = adm_55 - desl_55,
  saldo_56 = adm_56 - desl_56
)]

# Normalização: saldo como % de um mês "normal" de admissões do próprio município
ref <- painel[competencia >= as.Date(REF[1]) &
                competencia <= as.Date(REF[2]), .(
                  adm_ref = mean(adm),
                  adm_ref_55 = mean(adm_55),
                  adm_ref_56 = mean(adm_56)
                ), by = mun]
painel <- merge(painel, ref, by = "mun")
painel <- painel[adm_ref >= MIN_ADM]                              # corta municípios quase sem setor
painel[, `:=`(
  saldo_norm    = 100 * saldo / adm_ref,
  saldo_norm_55 = fifelse(adm_ref_55 > 0, 100 * saldo_55 / adm_ref_55, NA_real_),
  saldo_norm_56 = fifelse(adm_ref_56 > 0, 100 * saldo_56 / adm_ref_56, NA_real_)
)]

painel[, grupo := fifelse(mun == BELEM,
                          "Belém",
                          fifelse(mun %in% RM_RESTO, "RM (sem Belém)", "Interior"))]
painel[, `:=`(
  bel = as.integer(grupo == "Belém"),
  rm = as.integer(grupo == "RM (sem Belém)"),
  mes_ano = as.integer(format(competencia, "%m")),
  tempo   = format(competencia, "%Y-%m")
)]
for (f in names(FASES)) {
  lim <- as.Date(FASES[[f]])
  painel[, (f) := as.integer(competencia >= lim[1] &
                               competencia <= lim[2])]
  painel[, (paste0("Bel_", f)) := bel * get(f)]
  painel[, (paste0("RM_", f))  := rm  * get(f)]
}
painel[, fase := fifelse(prep == 1,
                         "2 Preparação",
                         fifelse(
                           evento == 1,
                           "3 Evento",
                           fifelse(desmob == 1, "4 Desmobilização", "1 Referência")
                         ))]
fwrite(painel, file.path(SAIDA, "painel_municipios.csv"))
message("  Municípios no painel: ", uniqueN(painel$mun), " (", painel[, uniqueN(mun), by = grupo][, paste(grupo, V1, collapse = "; ")], ")")

# ---- PASSO 4. Controles macro (Banco Central) ------------------------
message("PASSO 3/7 - Baixando câmbio, Selic e IPCA do Banco Central...")
# SGS 3698 = câmbio dólar (venda, média do mês); 4390 = Selic acumulada no mês; 433 = IPCA mensal
sgs <- rbcb::get_series(
  c(
    cambio = 3698,
    selic = 4390,
    ipca = 433
  ),
  start_date = as.Date("2021-12-01"),
  end_date = FIM + 31
)
macro <- Reduce(function(a, b)
  merge(a, b, by = "date"), lapply(sgs, as.data.table))
setnames(macro, "date", "competencia")
macro[, competencia := as.Date(format(competencia, "%Y-%m-01"))]
ivs <- fread("base_cop30_para.csv")[, .(competencia = as.Date(competencia), ln_ivs)]
macro <- merge(macro, ivs, by = "competencia", all.x = TRUE)
setorder(macro, competencia)
# Controles em VARIAÇÃO (e não em nível) para evitar regressão espúria (Granger e Newbold, 1974):
macro[, `:=`(
  d_cambio = 100 * (log(cambio) - shift(log(cambio))),
  # variação % do câmbio no mês
  d_ivs    = 100 * (ln_ivs - shift(ln_ivs))
)]             # variação % do volume de serviços
# selic e ipca já são taxas mensais (% no mês), então entram como estão
fwrite(macro, file.path(SAIDA, "macro.csv"))

# ---- PASSO 5. Tabela 1 e Figura 1 ------------------------------------
message("PASSO 4/7 - Tabela 1 (descritivas) e Figura 1...")
serie <- painel[, .(saldo = sum(saldo), adm_ref = sum(adm_ref)), by = .(grupo, competencia, fase)]
rm_int <- painel[grupo != "Interior", .(saldo = sum(saldo), adm_ref = sum(adm_ref)), by = .(competencia, fase)]
rm_int[, grupo := "RM inteira"]                                   # Belém + vizinhos: o efeito na região
serie <- rbind(serie, rm_int, use.names = TRUE)
serie[, saldo_norm := 100 * saldo / adm_ref]

# ---- Base limpa em Excel (abre direto no Excel; uma aba por tabela) ----
nomes_mun <- c(
  "150140" = "Belém",
  "150080" = "Ananindeua",
  "150442" = "Marituba",
  "150150" = "Benevides",
  "150635" = "Santa Bárbara do Pará",
  "150650" = "Santa Izabel do Pará",
  "150240" = "Castanhal"
)
xl_painel <- painel[, .(
  cod_ibge = mun,
  municipio = fifelse(mun %in% names(nomes_mun), nomes_mun[mun], NA_character_),
  grupo,
  mes = format(competencia, "%Y-%m"),
  fase,
  admissoes = adm,
  desligamentos = desl,
  saldo,
  saldo_55_alojamento = saldo_55,
  saldo_56_alimentacao = saldo_56,
  adm_media_referencia = round(adm_ref, 2),
  saldo_norm = round(saldo_norm, 2)
)]
setorder(xl_painel, grupo, cod_ibge, mes)
xl_series <- merge(serie, macro, by = "competencia", all.x = TRUE)[, .(
  grupo,
  mes = format(competencia, "%Y-%m"),
  fase,
  saldo,
  adm_media_referencia = round(adm_ref, 2),
  saldo_norm = round(saldo_norm, 2),
  var_volume_servicos = round(d_ivs, 3),
  var_cambio = round(d_cambio, 3),
  selic = selic,
  ipca = ipca
)]
setorder(xl_series, grupo, mes)
xl_macro <- macro[competencia >= INICIO, .(
  mes = format(competencia, "%Y-%m"),
  cambio,
  var_cambio = round(d_cambio, 3),
  selic,
  ipca,
  ln_volume_servicos = round(ln_ivs, 4),
  var_volume_servicos = round(d_ivs, 3)
)]
dicionario <- data.table(
  variavel = c(
    "cod_ibge",
    "municipio",
    "grupo",
    "mes",
    "fase",
    "admissoes",
    "desligamentos",
    "saldo",
    "saldo_55_alojamento",
    "saldo_56_alimentacao",
    "adm_media_referencia",
    "saldo_norm",
    "cambio",
    "var_cambio",
    "selic",
    "ipca",
    "ln_volume_servicos",
    "var_volume_servicos"
  ),
  descricao = c(
    "Código IBGE do município (6 dígitos)",
    "Nome (preenchido para Belém e região metropolitana)",
    "Belém; RM (sem Belém); Interior; RM inteira = Belém + vizinhos (só na aba series_grupos)",
    "Mês de competência (ano-mês)",
    "Referência (jan/22-jun/24); Preparação (jul/24-out/25); Evento (nov/25); Desmobilização (dez/25-jul/26)",
    "Admissões formais em CNAE 55 + 56",
    "Desligamentos formais em CNAE 55 + 56",
    "Admissões - desligamentos",
    "Saldo só em Alojamento (CNAE 55)",
    "Saldo só em Alimentação (CNAE 56)",
    "Média mensal de admissões de jan/2022 a jun/2024",
    "100 x saldo / adm_media_referencia (% de um mês normal de admissões)",
    "Dólar comercial, venda, média do mês (R$/US$)",
    "Variação % do câmbio no mês",
    "Selic acumulada no mês (% a.m.)",
    "IPCA, variação no mês (%)",
    "Log do índice de volume de serviços do Pará",
    "Variação % do volume de serviços no mês"
  ),
  fonte = c(
    rep(
      "Novo CAGED (MTE): ftp://ftp.mtps.gov.br/pdet/microdados/NOVO%20CAGED/ - arquivos MOV + FOR - EXC, UF 15, subclasse CNAE 55 e 56, por município e mês de competência",
      12
    ),
    "BCB-SGS série 3698: https://www3.bcb.gov.br/sgspub",
    "BCB-SGS série 3698 (calculada)",
    "BCB-SGS série 4390",
    "BCB-SGS série 433",
    "IBGE/PMS tabela 5906: https://sidra.ibge.gov.br/tabela/5906",
    "IBGE/PMS tabela 5906 (calculada)"
  )
)
writexl::write_xlsx(
  list(
    painel_municipios = xl_painel,
    series_grupos = xl_series,
    macro = xl_macro,
    dicionario = dicionario
  ),
  file.path(SAIDA, "base_helder.xlsx")
)
message("  Base limpa salva em: helder/base_helder.xlsx")

tab1 <- dcast(serie[, .(media = round(mean(saldo_norm), 1)), by = .(grupo, fase)], grupo ~ fase, value.var = "media")
print(tab1)
datasummary_df(tab1,
               output = file.path(SAIDA, "tabela1_descritivas.tex"),
               title = "Saldo normalizado médio por grupo e fase (\\% de um mês normal de admissões)")

fases_df <- data.frame(
  fase = names(FASES),
  ini = as.Date(sapply(FASES, `[`, 1)),
  fim = as.Date(sapply(FASES, `[`, 2)) + 27
)
g <- ggplot(serie, aes(competencia, saldo_norm, colour = grupo)) +
  geom_rect(
    data = fases_df,
    inherit.aes = FALSE,
    aes(
      xmin = ini,
      xmax = fim,
      ymin = -Inf,
      ymax = Inf,
      fill = fase
    ),
    alpha = 0.12
  ) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_line() +
  labs(
    x = NULL,
    y = "Saldo (% de um mês normal de admissões)",
    colour = "Grupo",
    fill = "Fase",
    caption = paste0(
      "Fonte: Novo CAGED (MTE), ftp://ftp.mtps.gov.br/pdet/microdados/NOVO%20CAGED/ ",
      "- arquivos MOV + FOR - EXC, UF 15 (Pará), CNAE 55 e 56,\nsomados por grupo de ",
      "municípios e mês de competência; saldo dividido pela média de admissões de jan/2022 a jun/2024."
    )
  ) +
  theme_minimal() + theme(legend.position = "bottom",
                          plot.caption = element_text(hjust = 0, size = 7))
ggsave(
  file.path(SAIDA, "figura1_grupos.png"),
  g,
  width = 9,
  height = 5,
  dpi = 200
)

# ---- PASSO 6. Modelo A: série de cada grupo + controles macro --------
message("PASSO 5/7 - Modelo A (séries por grupo, MQO + Newey-West)...")
serieA <- merge(serie, macro, by = "competencia")
serieA[, `:=`(mes_f = factor(as.integer(format(competencia, "%m"))), t = as.integer(factor(competencia)))]
for (f in names(FASES)) {
  lim <- as.Date(FASES[[f]])
  serieA[, (f) := as.integer(competencia >= lim[1] &
                               competencia <= lim[2])]
}
fA_sem <- saldo_norm ~ prep + evento + desmob + mes_f + t
fA_com <- saldo_norm ~ prep + evento + desmob + mes_f + t + d_ivs + d_cambio + selic + ipca
modA <- list()
for (gr in c("Belém", "RM (sem Belém)", "RM inteira", "Interior")) {
  modA[[paste(gr, "- sem macro")]] <- lm(fA_sem, data = serieA[grupo == gr])
  modA[[paste(gr, "- com macro")]] <- lm(fA_com, data = serieA[grupo == gr])
}
vc_nw <- function(m)
  sandwich::NeweyWest(m, prewhite = FALSE, adjust = TRUE)
nomesA <- c(
  prep = "Preparação",
  evento = "Evento (nov/2025)",
  desmob = "Desmobilização",
  d_ivs = "Var. % volume de serviços PA",
  d_cambio = "Var. % câmbio (R$/US$)",
  selic = "Selic (% no mês)",
  ipca = "IPCA (% no mês)"
)
for (ext in c("html", "tex")) {
  modelsummary(
    modA,
    vcov = vc_nw,
    stars = c(
      "*" = .1,
      "**" = .05,
      "***" = .01
    ),
    coef_map = nomesA,
    gof_map = c("nobs", "r.squared"),
    title = "Modelo A: saldo normalizado por grupo, MQO com dummies de mês e tendência",
    notes = "Erros-padrão de Newey-West entre parênteses.",
    output = file.path(SAIDA, paste0("tabela2_modeloA.", ext))
  )
}

# ---- PASSO 7. Modelo B: painel (efeito local x transbordamento) ------
message("PASSO 6/7 - Modelo B (painel de municípios, efeitos fixos)...")
RHS_B <- "Bel_prep + Bel_evento + Bel_desmob + RM_prep + RM_evento + RM_desmob | mun^mes_ano + tempo"
fB    <- as.formula(paste("saldo_norm ~", RHS_B))
fB_55 <- as.formula(paste("saldo_norm_55 ~", RHS_B))
fB_56 <- as.formula(paste("saldo_norm_56 ~", RHS_B))
modB <- list(
  "(1) Principal"         = feols(fB, data = painel, cluster = ~ mun),
  "(2) Só Alojamento (55)" = feols(fB_55, data = painel, cluster = ~ mun),
  "(3) Só Alimentação (56)" = feols(fB_56, data = painel, cluster = ~ mun),
  "(4) RM sem Castanhal"  = feols(fB, data = painel[mun != "150240"], cluster = ~
                                    mun),
  "(5) Out-dez em bloco"  = feols(fB, data = copy(painel)[, `:=`(
    Bel_prep   = bel * as.integer(
      competencia >= as.Date("2024-07-01") &
        competencia <= as.Date("2025-09-01")
    ),
    Bel_evento = bel * as.integer(
      competencia >= as.Date("2025-10-01") &
        competencia <= as.Date("2025-12-01")
    ),
    Bel_desmob = bel * as.integer(competencia >= as.Date("2026-01-01")),
    RM_prep    = rm  * as.integer(
      competencia >= as.Date("2024-07-01") &
        competencia <= as.Date("2025-09-01")
    ),
    RM_evento  = rm  * as.integer(
      competencia >= as.Date("2025-10-01") &
        competencia <= as.Date("2025-12-01")
    ),
    RM_desmob  = rm  * as.integer(competencia >= as.Date("2026-01-01"))
  )], cluster = ~ mun)
)
nomesB <- c(
  Bel_prep = "Belém x Preparação",
  Bel_evento = "Belém x Evento",
  Bel_desmob = "Belém x Desmobilização",
  RM_prep = "RM x Preparação",
  RM_evento = "RM x Evento",
  RM_desmob = "RM x Desmobilização"
)
for (ext in c("html", "tex")) {
  modelsummary(
    modB,
    stars = c(
      "*" = .1,
      "**" = .05,
      "***" = .01
    ),
    coef_map = nomesB,
    gof_map = c("nobs", "r.squared"),
    title = "Modelo B: efeito local (Belém) e transbordamento (RM), interior como controle",
    notes = "Efeitos fixos de município x mês do ano e de mês calendário. Erros agrupados por município.",
    output = file.path(SAIDA, paste0("tabela3_modeloB.", ext))
  )
}
print(etable(modB[["(1) Principal"]]))

# Placebo: cada município do interior faz o papel de "Belém"
message("PASSO 7/7 - Placebo (cada município do interior como se fosse Belém)...")
interior <- painel[grupo == "Interior", unique(mun)]
estima_bel <- function(u) {
  d <- copy(painel[grupo != "Belém" | u == BELEM])
  d[, b := as.integer(mun == u)]
  d[, `:=`(
    Bel_prep = b * prep,
    Bel_evento = b * evento,
    Bel_desmob = b * desmob
  )]
  coef(feols(fB, data = d))[c("Bel_prep", "Bel_evento", "Bel_desmob")]
}
plac <- rbindlist(lapply(c(BELEM, interior), function(u)
  as.data.table(as.list(estima_bel(
    u
  )))[, mun := u]))
bel  <- plac[mun == BELEM]
p_placebo <- sapply(c("Bel_prep", "Bel_evento", "Bel_desmob"), function(b)
  mean(abs(plac[[b]]) >= abs(bel[[b]])))
fwrite(plac, file.path(SAIDA, "placebo_belem.csv"))

cat("\n===== RESUMO =====\n")
cat(
  "Efeito de Belém (em % de um mês normal) e p-valor do placebo (",
  nrow(plac),
  " municípios):\n",
  sep = ""
)
print(round(data.table(
  fase = c("Preparação", "Evento", "Desmobilização"),
  efeito = unlist(bel[, .(Bel_prep, Bel_evento, Bel_desmob)]),
  p_placebo = p_placebo
), 3))
message("\nPRONTO! Tudo está na pasta: ", SAIDA)
print(data.table(fase = c("Preparação","Evento","Desmobilização"), efeito = round(unlist(bel[, .(Bel_prep, Bel_evento, Bel_desmob)]), 2), p_placebo = round(p_placebo, 3)))
sdref <- painel[fase == "1 Referência", .(sd_ref = sd(saldo_norm)), by = mun]
pl <- merge(plac, sdref, by = "mun")
for (b in c("Bel_prep","Bel_evento","Bel_desmob")) pl[, (paste0("z_", b)) := get(b) / sd_ref]
zb <- pl[mun == BELEM]
print(sapply(c("z_Bel_prep","z_Bel_evento","z_Bel_desmob"), function(b) round(mean(abs(pl[[b]]) >= abs(zb[[b]])), 3)))
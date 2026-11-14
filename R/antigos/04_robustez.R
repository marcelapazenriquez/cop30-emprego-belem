# =====================================================================
#  ROBUSTEZ do painel de capitais (Capítulo 4 da dissertação)
#  Rode DEPOIS do 03_belem_vs_capitais.R: este script lê o arquivo
#  painel_capitais.csv que o 03 salvou e não baixa nada.
#  Saída: robustez.csv (uma linha por especificação e fase)
# =====================================================================

for (p in c("data.table", "fixest")) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
library(data.table); library(fixest)

setwd("C:/Users/User/Documents/cop30")
painel <- fread("painel_capitais.csv")
painel[, competencia := as.IDate(competencia)]

TRATADA <- "Belém"
NORTE   <- c("Porto Velho", "Rio Branco", "Manaus", "Boa Vista", "Macapá", "Palmas")
base    <- painel[competencia >= as.IDate("2022-01-01") & cap != "Rio de Janeiro"]

# ---- Função: estima as fases para Belém e para cada capital (placebo) ----
estimar <- function(d, fases, excluir = character(0), tirar_belem = NULL, ate = NULL) {
  d <- copy(d[!cap %in% excluir])
  if (!is.null(ate))         d <- d[competencia <= as.IDate(ate)]
  if (!is.null(tirar_belem)) d <- d[!(cap == TRATADA & competencia %in% as.IDate(tirar_belem))]
  for (f in names(fases)) {
    lim <- as.IDate(fases[[f]])
    d[, (paste0("F_", f)) := as.integer(competencia >= lim[1] & competencia <= lim[2])]
  }
  fml <- as.formula(paste("saldo_norm ~", paste0("B_", names(fases), collapse = " + "),
                          "| cap^mes_ano + competencia"))
  uma <- function(u) {
    dd <- if (u == TRATADA) copy(d) else copy(d[cap != TRATADA])
    for (f in names(fases)) dd[, (paste0("B_", f)) := as.integer(cap == u) * get(paste0("F_", f))]
    m <- feols(fml, data = dd, warn = FALSE, notes = FALSE)
    as.data.table(as.list(coef(m)))[, cap := u]
  }
  res <- rbindlist(lapply(unique(d$cap), uma), fill = TRUE)
  bel <- res[cap == TRATADA]
  data.table(
    fase      = names(fases),
    efeito    = sapply(names(fases), function(f) bel[[paste0("B_", f)]]),
    p_placebo = sapply(names(fases), function(f) {
      b <- paste0("B_", f); mean(abs(res[[b]]) >= abs(bel[[b]]), na.rm = TRUE) }),
    unidades  = nrow(res)
  )
}

# ---- Especificações (a 1 é a principal, fixada antes de ver os resultados) ----
F_PRINCIPAL <- list(prep = c("2024-07-01", "2025-10-01"),
                    evento = c("2025-11-01", "2025-11-01"),
                    desmob = c("2025-12-01", "2026-07-01"))
specs <- list(
  "1 Principal"               = list(fases = F_PRINCIPAL),
  "2 Janela curta"            = list(fases = list(prep = c("2025-09-01", "2025-10-01"),
                                                  evento = c("2025-11-01", "2025-11-01"),
                                                  desmob = c("2025-12-01", "2026-07-01"))),
  "3 Out-dez em bloco"        = list(fases = list(prep = c("2024-07-01", "2025-09-01"),
                                                  evento = c("2025-10-01", "2025-12-01"),
                                                  desmob = c("2026-01-01", "2026-07-01"))),
  "4 Sem capitais do Norte"   = list(fases = F_PRINCIPAL, excluir = NORTE),
  "5 Sem nov/2024 em Belém"   = list(fases = F_PRINCIPAL, tirar_belem = "2024-11-01"),
  "6 Placebo no tempo (-2 anos)" = list(fases = list(prep = c("2022-07-01", "2023-10-01"),
                                                     evento = c("2023-11-01", "2023-11-01"),
                                                     desmob = c("2023-12-01", "2024-06-01")),
                                        ate = "2024-06-01")
)

saida <- rbindlist(lapply(names(specs), function(nm) {
  message("Estimando: ", nm)
  s <- specs[[nm]]
  r <- estimar(base, s$fases, excluir = s$excluir %||% character(0),
               tirar_belem = s$tirar_belem, ate = s$ate)
  r[, especificacao := nm]
}))
setcolorder(saida, "especificacao")
adm_ref_bel <- unique(painel[cap == TRATADA, adm_ref])
saida[, efeito_vagas_mes := round(efeito * adm_ref_bel / 100, 1)]
saida[, efeito := round(efeito, 2)][, p_placebo := round(p_placebo, 3)]
print(saida)
fwrite(saida, "robustez.csv")
message("\nPronto: robustez.csv. Efeito em % de um mês normal e em vagas por mês; p por placebo.")

# 00_rodar_tudo.R
# Roda os scripts em ordem. Abrir o projeto cop30.Rproj antes.
# O 01 so roda se ainda nao existir dados/processados/base_cop30_para.csv.
# Na primeira vez a leitura do CAGED demora; depois os agregados ficam salvos
# em dados/intermediarios/ e tudo roda em poucos minutos.

if (!file.exists("cop30.Rproj")) stop("abra o projeto cop30.Rproj antes de rodar")

scripts <- c("R/02_modelos_para.R", "R/03_capitais.R", "R/04_mecanismos.R")
if (!file.exists("dados/processados/base_cop30_para.csv")) scripts <- c("R/01_base_para.R", scripts)

for (s in scripts) {
  message("\n==== ", s, " ====")
  t0 <- Sys.time()
  source(s, local = new.env(), encoding = "UTF-8")
  message(s, " terminou em ", round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1), " min")
}

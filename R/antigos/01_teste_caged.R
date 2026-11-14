# =====================================================================
#  TESTE: baixa 1 mês do Novo CAGED (jan/2021) e mostra
#  admissões e desligamentos em Alojamento e Alimentação no Pará.
#  Para rodar: clique no botão "Source" (canto superior direito do script)
# =====================================================================

message("PASSO 1/5 - Carregando pacotes...")
for (p in c("data.table", "archive")) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
}
library(data.table)
library(archive)
options(timeout = 1800)

message("PASSO 2/5 - Criando a pasta de trabalho...")
pasta <- "C:/Users/User/Documents/cop30"
dir.create(pasta, showWarnings = FALSE)
setwd(pasta)
message("   Pasta: ", getwd())

message("PASSO 3/5 - Baixando o arquivo (pode levar alguns minutos)...")
url <- "ftp://ftp.mtps.gov.br/pdet/microdados/NOVO%20CAGED/2021/202101/CAGEDMOV202101.7z"
arq <- "CAGEDMOV202101.7z"
if (!file.exists(arq)) download.file(url, arq, mode = "wb")
message("   Baixado: ", round(file.size(arq) / 1e6, 1), " MB")

message("PASSO 4/5 - Descompactando e lendo...")
archive_extract(arq, dir = "teste_extraido")
txt <- list.files("teste_extraido", pattern = "\\.txt$", full.names = TRUE,
                  ignore.case = TRUE, recursive = TRUE)[1]
dados <- fread(txt, sep = ";", colClasses = "character", showProgress = FALSE)
message("   Linhas no arquivo (Brasil todo): ", nrow(dados))
message("   Colunas: ", paste(names(dados), collapse = ", "))

message("PASSO 5/5 - Filtrando Pará e CNAE 55/56...")
nomes <- tolower(names(dados))
col_uf    <- names(dados)[grep("^uf$", nomes)]
col_sub   <- names(dados)[grep("^subclasse$", nomes)]
col_saldo <- names(dados)[grep("^saldomovim", nomes)]

para <- dados[as.integer(get(col_uf)) == 15]
para[, divisao := substr(sprintf("%07d", as.integer(get(col_sub))), 1, 2)]
para <- para[divisao %in% c("55", "56")]

resultado <- para[, .(admissoes     = sum(get(col_saldo) == "1"),
                      desligamentos = sum(get(col_saldo) == "-1")),
                  by = divisao]
resultado[, saldo := admissoes - desligamentos]

message("\n===== RESULTADO: Pará, jan/2021 =====")
print(resultado)
message("\nSe apareceu a tabela acima, o teste FUNCIONOU.")

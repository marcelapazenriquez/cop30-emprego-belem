# COP30 e emprego formal em Belém

Dissertação de mestrado em Economia (PPGE/UFF). Marcela Paz da Silva Enriquez.

Pergunta: a COP30 (Belém, novembro de 2025) mudou o emprego formal em
alojamento e alimentação (CNAE 55 e 56) em Belém, e o efeito ficou depois
do evento?

A variável principal é o saldo do Novo CAGED (admissões menos desligamentos)
de cada município ou capital em cada mês, dividido pela média de admissões
do próprio município entre jan/2022 e jun/2024. A estratégia é diferenças em
diferenças com efeitos fixos, em duas bases:

- municípios do Pará: Belém, vizinhos da RM e interior (efeito local e transbordamento)
- capitais: Belém contra as outras 25 capitais, sem o Rio de Janeiro (G20 em nov/2024)

Como só uma cidade é tratada, a inferência é feita por placebo (cada
município ou capital no lugar de Belém) e controle sintético.

Fases:

    referência       jan/2022 a jun/2024
    preparação       jul/2024 a out/2025
    evento           nov/2025
    desmobilização   dez/2025 a jul/2026


## Pastas

    R/
      00_rodar_tudo.R        roda 01 (se preciso), 02, 03 e 04 em ordem
      01_base_para.R         baixa o CAGED, monta a série do Pará e o índice de volume de serviços
      02_modelos_para.R      painel de municípios do Pará, modelos A, B e C, placebo, estudo de evento, ADF
      03_capitais.R          Belém x capitais: DiD com placebo, robustez, estudo de evento, controle sintético
      04_mecanismos.R        tipo de vínculo (prazo determinado, intermitente) e salário de admissão
      funcoes.R              funções auxiliares usadas pelos scripts
      antigos/               versões anteriores (caminhos fixos no meu computador, só para registro)
    dados/
      brutos/                arquivos .7z do Novo CAGED (não vão para o GitHub)
      intermediarios/        agregados mensais em .rds (não vão para o GitHub)
      processados/           bases prontas (.csv e .xlsx)
    resultados/
      tabelas/
      figuras/
    texto/
      paper_helder/          artigo para Macroeconomia e Finanças (Prof. Helder Ferreira de Mendonça)
      atividade4/            métodos, Seminário de Dissertação
      resumo_seminario/      resumo para o Seminário de Dissertação
      dissertacao/           rascunho dos capítulos
      notas/                 notas de planejamento


## Como rodar

1. Abrir `cop30.Rproj` no RStudio (isso define a pasta do projeto como diretório de trabalho).
2. Rodar os scripts em ordem: `01_base_para.R`, `02_modelos_para.R`, `03_capitais.R`.

Os pacotes que faltarem são instalados no começo de cada script.

Na primeira vez o `01` baixa os microdados do CAGED de jan/2021 a jul/2026
(três arquivos por mês: MOV, FOR e EXC), o que leva tempo e ocupa alguns GB.
Depois disso os agregados ficam salvos em `dados/intermediarios/` e os
scripts rodam em poucos minutos. Quem já tiver os .7z baixados pode
copiá-los para `dados/brutos/` antes de rodar, e o download é pulado.

O `02` e o `03` também acessam a internet: séries do Banco Central (pacote
`rbcb`) e IPCA do SIDRA (pacote `sidrar`).


## Versão dos resultados

`dados/processados/base_cop30_para.csv` saiu da primeira versão do 01
(`02_base_completa.R`, o mesmo código com outros caminhos). Os demais
arquivos em `dados/processados/` e `resultados/` (nov/2026) foram gerados
por `R/antigos/05_helder_v1.R`. Nessa versão:

- o modelo A usa como controles a variação do volume de serviços do Pará
  (PMS), a variação do câmbio, a Selic e o IPCA;
- não há modelo C, estudo de evento nem teste ADF.

O `02_modelos_para.R` é a versão seguinte: troca o volume de serviços do
Pará pelo IBC-Br (o volume do Pará é afetado pela própria COP), coloca a
Selic em variação e acrescenta o modelo C, o estudo de evento e o ADF.
Rodar o 02 sobrescreve esses arquivos. O modelo B e o placebo são iguais
nas duas versões.

Os resultados das capitais (`capitais_*.csv` e figuras) saem do `03_capitais.R`
e os de mecanismos (`mecanismos_*.csv`, `mecanismos_belem.png`) do `04_mecanismos.R`.


## Resultados preliminares (Belém x 25 capitais, alojamento + alimentação)

Efeito medido em pontos do saldo normalizado (saldo do mês como % da média
mensal de admissões de referência); p-valor pelo placebo entre as 26
capitais (o menor possível é 1/26 = 0,038).

    fase             efeito    p (placebo)
    preparação         6,8      0,077
    evento (nov/25)   27,3      0,038
    desmobilização    -5,2      0,231

- Efeito acumulado até nov/2025: cerca de 1.060 vagas formais a mais em Belém
  (p = 0,077 simples; 0,038 ponderado). Em jul/2026 o acumulado cai para cerca
  de 690 vagas e deixa de ser significativo (p = 0,154).
- O efeito se concentra em alojamento (CNAE 55): +118 pontos no mês do evento.
- Mecanismos: no mês do evento, sobem as admissões e os desligamentos por
  término de contrato no alojamento, e o salário de admissão na alimentação
  fica cerca de 7% acima (p = 0,038).

Fontes: `resultados/tabelas/capitais_did.csv`, `capitais_acumulado.csv`,
`mecanismos_did.csv`. Resultados em revisão, sujeitos a mudança.


## Fontes

- Novo CAGED, microdados (MTE/PDET): ftp://ftp.mtps.gov.br/pdet/microdados/NOVO%20CAGED/
  arquivos MOV + FOR - EXC, por mês de competência.
- Pesquisa Mensal de Serviços, IBGE, tabela 5906: https://sidra.ibge.gov.br/tabela/5906
- IPCA por item, IBGE, tabela 7060: https://sidra.ibge.gov.br/tabela/7060
- Banco Central, SGS (https://www3.bcb.gov.br/sgspub): câmbio 3698, Selic 4390, IPCA 433, IBC-Br 24364.


## Observações

- O CAGED cobre só o emprego com carteira assinada (inclui prazo determinado,
  temporário, intermitente e parcial). Informais não aparecem.
- A RM de Belém usada aqui inclui Castanhal; o modelo B tem uma coluna sem Castanhal.
- Os textos em `texto/` compilam com pdfLaTeX. O `paper_helder.tex` usa a figura
  `figura1_grupos.png`, que fica na mesma pasta (cópia de `resultados/figuras/`).

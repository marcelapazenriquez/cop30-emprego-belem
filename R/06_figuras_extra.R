# 06_figuras_extra.R
# Duas figuras a partir do estudo de evento das capitais (rodar depois do 03):
#   capitais_event_study_faixa_alojamento.png  efeito mensal de Belem (pontos) e faixa das capitais placebo
#   capitais_dispersao_alojamento.png          cada capital: ruido antes da COP x efeito em nov/2025
# Com uma so unidade tratada nao ha intervalo de confianca convencional; a faixa cinza mostra
# onde caem os efeitos estimados das outras capitais quando cada uma e tratada como se fosse Belem.
#
# entrada: dados/processados/capitais_event_study.csv (gerado pelo 03_capitais.R)

library(data.table); library(ggplot2)
if (!file.exists("cop30.Rproj")) stop("abra o projeto cop30.Rproj antes de rodar")
FIG <- "resultados/figuras"

TRATADA <- "Belém"
Y       <- "saldo_norm_55"                      # Alojamento
PREP    <- as.Date("2024-07-01")
EVENTO  <- as.Date("2025-11-01")
FONTE <- paste0("Fonte: Novo CAGED (MTE), ftp://ftp.mtps.gov.br/pdet/microdados/NOVO%20CAGED/ - ",
                "arquivos MOV + FOR - EXC, CNAE 55, 26 capitais (exceto Rio de Janeiro),\nsomados por ",
                "capital e mês de competência; saldo dividido pela média de admissões de jan/2022 a jun/2024. ",
                "Efeitos fixos de capital x mês do ano\ne de mês calendário; efeito de cada mês em relação a 2022.")

ev <- fread("dados/processados/capitais_event_study.csv")[variavel == Y]
ev[, competencia := as.Date(competencia)]
ev[, fase := fifelse(competencia < PREP, "Antes", fifelse(competencia < EVENTO, "Preparação",
                     fifelse(competencia == EVENTO, "Evento", "Desmobilização")))]
ev[, fase := factor(fase, levels = c("Antes", "Preparação", "Evento", "Desmobilização"))]

# ---- figura 1: Belem contra a faixa dos placebos ----------------------
faixa <- ev[unidade != TRATADA, .(lo = quantile(efeito, 0.05, na.rm = TRUE),
                                  hi = quantile(efeito, 0.95, na.rm = TRUE)), by = competencia]
g1 <- ggplot() +
  geom_ribbon(data = faixa, aes(competencia, ymin = lo, ymax = hi), fill = "grey85") +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = PREP, linetype = 2) +
  geom_vline(xintercept = EVENTO, linetype = 3) +
  geom_point(data = ev[unidade == TRATADA], aes(competencia, efeito, shape = fase), size = 1.8) +
  scale_shape_manual(values = c(1, 16, 17, 15)) +
  labs(x = NULL, y = "Efeito no saldo, Alojamento (% de um mês normal)", shape = NULL,
       caption = paste0(FONTE, " Pontos: Belém. Faixa cinza: entre 5% e 95% dos efeitos das 25 outras ",
                        "capitais (placebo). Linha tracejada:\ninício da preparação (jul/2024); ",
                        "linha pontilhada: nov/2025.")) +
  theme_minimal() + theme(legend.position = "bottom", plot.caption = element_text(hjust = 0, size = 7))
ggsave(file.path(FIG, "capitais_event_study_faixa_alojamento.png"), g1, width = 9, height = 5, dpi = 200)

# ---- figura 2: dispersao, ruido antes x efeito no evento --------------
# eixo x: quanto o efeito estimado de cada capital oscila antes da preparacao (RMSPE pre)
# eixo y: efeito estimado em nov/2025. Uma capital barulhenta pode ter efeito grande por acaso;
# Belem se destaca se fica longe das outras no eixo y sem estar longe no eixo x.
disp <- ev[, .(rmspe_pre = sqrt(mean(efeito[competencia < PREP]^2, na.rm = TRUE)),
               efeito_nov = efeito[competencia == EVENTO]), by = unidade]
disp[, belem := unidade == TRATADA]
fwrite(disp, "resultados/tabelas/capitais_dispersao_alojamento.csv")
g2 <- ggplot(disp, aes(rmspe_pre, efeito_nov)) +
  geom_hline(yintercept = 0, colour = "grey60") +
  geom_point(aes(shape = belem), size = 2) +
  geom_text(aes(label = unidade), size = 2.5, vjust = -0.9) +
  scale_shape_manual(values = c(`FALSE` = 1, `TRUE` = 16), guide = "none") +
  labs(x = "Oscilação do efeito antes da preparação, jan/2023 a jun/2024 (raiz do erro quadrático médio)",
       y = "Efeito em nov/2025 (% de um mês normal)",
       caption = paste0(FONTE, " Cada ponto é uma capital estimada como se fosse a tratada; ",
                        "ponto cheio: Belém.")) +
  theme_minimal() + theme(plot.caption = element_text(hjust = 0, size = 7))
ggsave(file.path(FIG, "capitais_dispersao_alojamento.png"), g2, width = 9, height = 5.5, dpi = 200)

print(disp[order(-abs(efeito_nov))])

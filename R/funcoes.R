# funcoes.R
# Funcoes de estimacao usadas pelos scripts 02, 03 e 04.
# Todas recebem um data.table com as colunas:
#   unidade (municipio ou capital), competencia (Date), mes_ano (1-12), tempo ("2025-11")
# e a variavel de resultado y. Os efeitos fixos sao sempre unidade x mes do ano e mes calendario.
#
# Inferencia com uma so unidade tratada: a mesma regressao e rodada com cada unidade de
# controle no lugar de Belem (placebo) e o p-valor e a posicao de Belem nessa distribuicao
# (Abadie, Diamond e Hainmueller, 2010; Conley e Taber, 2011).

# fracao de unidades com estatistica >= a de Belem, em modulo
p_rank <- function(x, x_tratada) round(mean(abs(x) >= abs(x_tratada), na.rm = TRUE), 3)

# efeito mes a mes da unidade u: uma dummy (unidade == u) x (mes == m) para cada mes em `meses`
coef_mensal <- function(d, y, u, meses, prefixo) {
  cols <- paste0(prefixo, format(meses, "%Y_%m"))
  dd <- copy(d[!is.na(get(y))])
  for (k in seq_along(meses)) dd[, (cols[k]) := as.integer(unidade == u & competencia == meses[k])]
  f <- as.formula(paste(y, "~", paste(cols, collapse = " + "), "| unidade^mes_ano + tempo"))
  cf <- tryCatch(coef(feols(f, data = dd, notes = FALSE, warn = FALSE)),
                 error = function(e) numeric(0))
  data.table(unidade = u, competencia = meses, efeito = unname(cf[cols]))
}

# o mesmo para Belem e para cada unidade de controle (placebo).
# No placebo Belem sai da amostra, para nao contaminar o grupo de comparacao.
coef_mensal_todas <- function(d, y, tratada, meses, prefixo) {
  controles <- setdiff(unique(d$unidade), tratada)
  rbindlist(lapply(c(tratada, controles), function(u) {
    dd <- if (u == tratada) d else d[unidade != tratada]
    coef_mensal(dd, y, u, meses, prefixo)
  }))
}

# desvio-padrao de y de cada unidade no periodo de referencia (para o placebo ponderado)
sd_referencia <- function(d, y, ref) {
  d[competencia >= as.Date(ref[1]) & competencia <= as.Date(ref[2]),
    .(sd_ref = sd(get(y), na.rm = TRUE)), by = unidade]
}

# estudo de evento: dummies mes a mes desde `inicio` (o periodo anterior e a base).
# Resume as tendencias pre-evento de Belem contra as do placebo:
#   media_pre    media dos efeitos antes de `inicio_pos` (deveria ficar perto de zero)
#   p_media_pre  fracao de unidades com media_pre maior que a de Belem (p alto = Belem nao destoa antes)
#   razao        RMSPE depois / RMSPE antes; p_razao = fracao de unidades com razao >= a de Belem
#                (teste de Abadie, Diamond e Hainmueller, 2010)
estudo_evento <- function(d, y, tratada, inicio, fim, inicio_pos) {
  meses <- seq(as.Date(inicio), as.Date(fim), by = "month")
  ev <- coef_mensal_todas(d, y, tratada, meses, prefixo = "ev_")
  pos <- as.Date(inicio_pos)
  s <- ev[, .(media_pre = mean(efeito[competencia < pos], na.rm = TRUE),
              rmspe_pre = sqrt(mean(efeito[competencia < pos]^2, na.rm = TRUE)),
              rmspe_pos = sqrt(mean(efeito[competencia >= pos]^2, na.rm = TRUE))), by = unidade]
  s[, razao := rmspe_pos / rmspe_pre]
  b <- s[unidade == tratada]
  resumo <- data.table(variavel = y,
                       media_pre_belem = round(b$media_pre, 2),
                       p_media_pre = p_rank(s$media_pre, b$media_pre),
                       rmspe_pre_belem = round(b$rmspe_pre, 2),
                       rmspe_pos_belem = round(b$rmspe_pos, 2),
                       razao_belem = round(b$razao, 2),
                       p_razao = round(mean(s$razao >= b$razao, na.rm = TRUE), 3),
                       unidades = sum(!is.na(s$razao)))
  list(coef = ev[, variavel := y], resumo = resumo)
}

# efeito acumulado: dummies mes a mes so a partir de `inicio_pos` (base = todo o periodo antes).
# acum_pct = soma dos efeitos mensais, em % de um mes normal de admissoes.
# Ex.: acum_pct = 150 quer dizer 1,5 mes normal de admissoes a mais no estoque de empregos.
efeito_acumulado <- function(d, y, tratada, inicio_pos, fim, sd_ref) {
  meses <- seq(as.Date(inicio_pos), as.Date(fim), by = "month")
  ev <- coef_mensal_todas(d, y, tratada, meses, prefixo = "ac_")
  setorder(ev, unidade, competencia)
  ev[, acum_pct := cumsum(fcoalesce(efeito, 0)), by = unidade]
  ev <- merge(ev, sd_ref, by = "unidade", all.x = TRUE)
  ev[, variavel := y]
  ev
}

# tabela do efeito acumulado de Belem em algumas datas, em % e em vagas, com p do placebo
tabela_acumulado <- function(ac, tratada, datas, adm_ref_belem) {
  rbindlist(lapply(datas, function(dt) {
    x <- ac[competencia == as.Date(dt)]
    b <- x[unidade == tratada]
    data.table(variavel = ac$variavel[1],
               ate = format(as.Date(dt), "%m/%Y"),
               acumulado_pct = round(b$acum_pct, 1),
               vagas = round(b$acum_pct * adm_ref_belem / 100),
               p_simples = p_rank(x$acum_pct, b$acum_pct),
               p_ponderado = p_rank(x$acum_pct / x$sd_ref, b$acum_pct / b$sd_ref),
               unidades = sum(!is.na(x$acum_pct)))
  }))
}

# efeito por fase (dummies Belem x fase), com placebo; `fases` e uma lista de pares de datas
did_fases <- function(d, y, tratada, fases, sd_ref) {
  d <- copy(d[!is.na(get(y))])
  for (f in names(fases)) {
    lim <- as.Date(fases[[f]])
    d[, (paste0("F_", f)) := as.integer(competencia >= lim[1] & competencia <= lim[2])]
  }
  fml <- as.formula(paste(y, "~", paste0("B_", names(fases), collapse = " + "), "| unidade^mes_ano + tempo"))
  uma <- function(u) {
    dd <- if (u == tratada) copy(d) else copy(d[unidade != tratada])
    for (f in names(fases)) dd[, (paste0("B_", f)) := as.integer(unidade == u) * get(paste0("F_", f))]
    cf <- tryCatch(coef(feols(fml, data = dd, notes = FALSE, warn = FALSE)), error = function(e) numeric(0))
    v <- unname(cf[paste0("B_", names(fases))])
    names(v) <- paste0("B_", names(fases))
    as.data.table(as.list(v))[, unidade := u]
  }
  controles <- setdiff(unique(d$unidade), tratada)
  res <- rbindlist(lapply(c(tratada, controles), uma))
  res <- merge(res, sd_ref, by = "unidade", all.x = TRUE)
  bel <- res[unidade == tratada]
  rbindlist(lapply(names(fases), function(f) {
    b <- paste0("B_", f)
    data.table(variavel = y, fase = f,
               efeito = round(bel[[b]], 3),
               p_simples = p_rank(res[[b]], bel[[b]]),
               p_ponderado = p_rank(res[[b]] / res$sd_ref, bel[[b]] / bel$sd_ref),
               unidades = sum(!is.na(res[[b]])))
  }))
}

# grafico Belem (preto) contra o placebo (cinza)
grafico_placebo <- function(dados, tratada, ycol, ylab, fonte, arquivo,
                            inicio_prep = as.Date("2024-07-01"), evento = as.Date("2025-11-01")) {
  g <- ggplot() +
    annotate("rect", xmin = inicio_prep, xmax = evento, ymin = -Inf, ymax = Inf, fill = "grey92") +
    geom_line(data = dados[unidade != tratada], aes(competencia, .data[[ycol]], group = unidade),
              colour = "grey70", linewidth = 0.3) +
    geom_line(data = dados[unidade == tratada], aes(competencia, .data[[ycol]]),
              colour = "black", linewidth = 0.9) +
    geom_hline(yintercept = 0, linetype = 2) +
    geom_vline(xintercept = evento, linetype = 3) +
    labs(x = NULL, y = ylab, caption = fonte) +
    theme_minimal() + theme(plot.caption = element_text(hjust = 0, size = 7))
  ggsave(arquivo, g, width = 9, height = 4.8, dpi = 200)
  invisible(g)
}

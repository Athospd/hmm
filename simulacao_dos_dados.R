library(lubridate)
library(tidyverse)
library(expm)

gera_serie_de_states <- function(t, matriz_de_transicao) {
  accumulate(t, ~{
    probabilidades <- matriz_de_transicao[.x, ]
    state <- rmultinom(1, 1, prob = probabilidades) %>% as.logical() %>% which()
    if(state > 2) browser()
    return(state)
  }, .init = 1)[-1] - 1
}

transicao_oos <- matrix(c(0.988, 0.012,
                          0.002, 0.998), 2, 2)
#transicao_promocao <- diag(2)
transicao_promocao <- matrix(c(0.990, 0.010,
                               0.001, 0.999), 2, 2)


dados <- tibble(
  dia = seq.Date(as.Date("2000-01-01"), as.Date("2019-12-31"), "1 day"),
  t = yday(dia),
  sku = sample(c("sku123", "sku456", "sku789"), length(t), replace = TRUE),
  tem_promocao = gera_serie_de_states(t,  matriz_de_transicao = transicao_promocao),
  oos = gera_serie_de_states(t, matriz_de_transicao = transicao_oos)
) %>%
  mutate(
    preditor_linear = model.matrix(~ sku + tem_promocao + oos, data = .) %*% c(4, 0, 0, 0, -3),
    tickets = rpois(nrow(.), lambda = exp(preditor_linear))
  )



dados %>%
  filter(sku %in% "sku123") %>%
  gather("serie", "valor", tem_promocao, oos, tickets) %>%
  ggplot(aes(x = dia, y = valor, colour = serie)) +
  geom_line() +
  facet_grid(serie ~ sku, scales = "free_y")


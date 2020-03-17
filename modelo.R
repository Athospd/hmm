library(tidyverse)
library(depmixS4)

glimpse(dados)

set.seed(1)
mod <- depmix(
  response = tickets ~ sku + log(preço - mean(preço)), 
  transition = ~ sku + loja,
  data = bind_rows(dados), 
  nstates = 2, 
  family = poisson(),
  respstart = c(5, 5, 5, 5, 5, 5),
  trstart = rbind(transicao_oos, transicao_oos, transicao_oos),
  instart = transicao_oos[1,]
)

mod

fm <- fit(mod)
summary(fm)
transicao_oos

dados_com_predicoes <- dados %>%
  mutate(
    oos_pred = posterior(fm)$state
  )

dados_com_predicoes %>%
  count(oos, oos_pred) %>%
  spread(oos, n, fill = 0)



# restrição de interceptos crescentes.
# parametrização ordinal dos logitos.
# epsilon de quando a loja só tem vendas zeradas no estado de OOS.
# binomial em vez de poisson (pra lojas de N pequeno)

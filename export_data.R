library(tidyverse)

load("dados_sent_seca.RData")

munic_names <- tab5 |>
  select(cod_munic, sigla, nome_munic) |>
  distinct()

write_rds(munic_names, file = "../sent_seca_data/munic_names.rds")

write_rds(mes, file = "../sent_seca_data/mes.rds")

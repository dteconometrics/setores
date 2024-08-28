
# Pacotes  ----------------------------------------------------------------


# Carregamento de pacotes
library(tidyverse)
library(feasts)
library(patchwork)
library(sidrar)
library(skimr)




# Coleta ------------------------------------------------------------------


## Pim 

pim_pf_pe = sidrar::get_sidra(api= '/t/8888/n3/26/v/12606/p/all/c544/56689,129314,129316,129317,129318,129320,129324,129331,129332,129333,129334,129336,129338,129339/d/v12606%205') %>% 
  dplyr::mutate(date = parse_date(`Mês (Código)`, format='%Y%m')) %>% 
  dplyr::select(date, "Seções e atividades industriais (CNAE 2.0)", Valor) %>% 
  spread("Seções e atividades industriais (CNAE 2.0)", Valor) %>%
  dplyr::select(-'3.13 Fabricação de produtos têxteis', -'3.17 Fabricação de celulose, papel e produtos de papel',-'3.22 Fabricação de produtos de borracha e de material plástico', -'3.24 Metalurgia', -'3.27 Fabricação de máquinas, aparelhos e materiais elétricos',-'3.29 Fabricação de veículos automotores, reboques e carrocerias', -'3.30 Fabricação de outros equipamentos de transporte, exceto veículos automotores') %>% 
  dplyr::select(-'3.20 Fabricação de produtos químicos') %>% 
  as_tibble() %>% 
  drop_na()


pim_pf = pim_pf_pe %>% 
  dplyr::select(date, "1 Indústria geral") %>% 
  dplyr::rename(pim_pf = "1 Indústria geral")


## PMS

names <- c('date', 'receita', 'volume', 'receita_sa', 'volume_sa')

tabpms_pe = '/t/5906/n3/26/v/7167,7168/p/all/c11046/all/d/v7167%205,v7168%205' %>% 
  sidrar::get_sidra(api=.) %>% 
  mutate(date = parse_date(`Mês (Código)`, format = '%Y%m')) %>% 
  dplyr::select(date, "Variável", "Tipos de índice", Valor) %>% 
  pivot_wider(id_cols = date, 
              names_from = c("Variável", "Tipos de índice"),
              values_from = Valor) %>% 
  `colnames<-`(names) %>% 
  dplyr::select(date, receita, receita_sa, volume, volume_sa) %>% 
  as_tibble()

pms = tabpms_pe %>% 
  dplyr::select(date, receita, volume)



## PMC 




# Tratamento  -------------------------------------------------------------



data_inicio <- lubridate::as_date("2004-01-01")

# Índice Nacional de Preços ao Consumidor Amplo (IPCA) - % a.m. - IBGE

# Cruzar as tabelas 

dados <- purrr::reduce(
  .x = list(pim_pf, pms),
  .f = dplyr::left_join,
  by = "date"
) %>% 
  drop_na()


## Criar objeto de dados de classe tsibble


dados_ts <- dados %>% 
  dplyr::mutate(data = tsibble::yearmonth(date)) %>% 
  tsibble::as_tsibble(index = data) %>% 
  dplyr::select(data, pim_pf, receita, volume)



# Gráficos  ---------------------------------------------------------------

lplot_pim <- dados_ts %>% 
  autoplot()


lplot_pms <- dados_ts |> 
  dplyr::select(data, receita, volume) |> 
  autoplot()

(lplot_pim / lplot_pms)



# Estatística Descritia  --------------------------------------------------

skimr::skim(dados)





# Histograma  -------------------------------------------------------------

# Gráfico de histograma do IPCA
hist_pim <- dados %>%
  ggplot2::ggplot() +
  ggplot2::aes(x = pim_pf) +
  ggplot2::geom_histogram()

# Gráfico de histograma do IPCA
hist_pms <- dados %>%
  ggplot2::ggplot() +
  ggplot2::aes(x = receita) +
  ggplot2::geom_histogram()

(hist_pim + hist_pms)


# Sazonalidade ------------------------------------------------------------

# Gráfico de sazonalidade
feasts::gg_subseries(
  data   = dados_ts,
  y      = pim_pf,
  labels = "right"
)

feasts::gg_subseries(
  data   = dados_ts,
  y      = receita,
  labels = "right"
)



# Decomposição  -----------------------------------------------------------

dados_ts |> 
  dplyr::select(data, pim_pf) |> 
  fabletools::model(
    feasts::classical_decomposition(type = "additive")
  ) |> 
  fabletools::components() |> 
  autoplot() +
  ggplot2::labs(title = "Decomposição Clássica Aditiva da variação mensal da PIM")


dados_ts |> 
  dplyr::select(data, receita) |> 
  fabletools::model(
    feasts::classical_decomposition(type = "additive")
  ) |> 
  fabletools::components() |> 
  autoplot() +
  ggplot2::labs(title = "Decomposição Clássica Aditiva da variação mensal da receita")




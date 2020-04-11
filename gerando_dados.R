corrigir_data <- function(dados) {
  
  dados <- dados %>% 
    mutate(
      date = gsub(".20", "/2020", date),
      date = gsub("\\.", "/", date),
      date = gsub("X", "0", date),
      date = gsub("/", "-", date),
      date = as.Date(date, "%m-%d-%Y")
    )
  
  return(dados)
  
}

gerandos_dados_mundo <- function() {
  
  link_confirmados <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
  link_obitos <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
  link_curados <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv"
  
  df_casos_confirmados <- 
    read.csv(link_confirmados) %>% 
    pivot_longer(
      cols = contains(".20"),
      names_to = "date",
      values_to = "casos_confirmados",
      values_drop_na = TRUE
    ) %>% 
    corrigir_data()
  
  df_mortes <- 
    read.csv(link_obitos) %>% 
    pivot_longer(
      cols = contains(".20"),
      names_to = "date",
      values_to = "mortes",
      values_drop_na = TRUE
    ) %>% 
    corrigir_data()
  
  df_casos_curados <- 
    read.csv(link_curados) %>% 
    pivot_longer(
      cols = contains(".20"),
      names_to = "date",
      values_to = "casos_curados",
      values_drop_na = TRUE
    ) %>% 
    corrigir_data()
  
  dados <- left_join(
    df_casos_confirmados, 
    df_mortes,
    by = c("Province.State", "Country.Region", "Lat", "Long", "date")
  ) %>% 
    left_join(
      df_casos_curados,
      by = c("Province.State", "Country.Region", "Lat", "Long", "date")
    ) %>% 
    filter(!is.na(date)) %>% 
    mutate(
      Mes = as.yearmon(date)
    )
  
  saveRDS(dados, "dados/dados_mundo.rds", version = 2)
  
}


gerando_dados_brasil <- function() {
  
  link_dados <- "https://brasil.io/dataset/covid19/caso?format=csv"
  dados_brasil <- read.csv(link_dados)
  
  saveRDS(dados_brasil, "dados/dados_brasil.rds", version = 2)
  
}

gerandos_dados_mundo()
gerando_dados_brasil()



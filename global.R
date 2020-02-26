# install.packages(c("tidyverse", "shiny", "tablerDash", "shinycssloaders", "zoo"))

library(tidyverse)
library(shiny)
library(tablerDash)
library(shinycssloaders)
library(zoo)
library(highcharter)

obter_dados <- function() {
  
  df_casos_confirmados <- 
    read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv") %>% 
    pivot_longer(
      cols = contains(".20"),
      names_to = "data",
      values_to = "casos_confirmados",
      values_drop_na = TRUE
    ) %>% 
    mutate(
      data = gsub(".20", "/2020", data),
      data = gsub("\\.", "/", data),
      data = gsub("X", "0", data),
      data = gsub("/", "-", data),
      data = as.Date(data, "%m-%d-%Y")
    )
  
  df_mortes <- 
    read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv") %>% 
    pivot_longer(
      cols = contains(".20"),
      names_to = "data",
      values_to = "mortes",
      values_drop_na = TRUE
    ) %>% 
    mutate(
      data = gsub(".20", "/2020", data),
      data = gsub("\\.", "/", data),
      data = gsub("X", "0", data),
      data = gsub("/", "-", data),
      data = as.Date(data, "%m-%d-%Y")
    )
  
  df_casos_curados <- 
    read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv") %>% 
    pivot_longer(
      cols = contains(".20"),
      names_to = "data",
      values_to = "casos_curados",
      values_drop_na = TRUE
    ) %>% 
    mutate(
      data = gsub(".20", "/2020", data),
      data = gsub("\\.", "/", data),
      data = gsub("X", "0", data),
      data = gsub("/", "-", data),
      data = as.Date(data, "%m-%d-%Y")
    )
  
  dados <- left_join(
    df_casos_confirmados, 
    df_mortes,
    by = c("Province.State", "Country.Region", "Lat", "Long", "data")
    ) %>% 
    left_join(
      df_casos_curados,
      by = c("Province.State", "Country.Region", "Lat", "Long", "data")
    ) %>% 
    filter(!is.na(data)) %>% 
    mutate(
      Mes = as.yearmon(data)
    )

  return(dados)
}

obter_ultima_data <- function(dados) {
  dados %>%
    arrange(data) %>%
    pull(data) %>%
    unique() %>%
    last()
}
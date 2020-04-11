# install.packages(c("tidyverse", "shiny", "tablerDash", "shinycssloaders", "zoo"))

library(tidyverse)
library(shiny)
library(tablerDash)
library(shinycssloaders)
library(zoo)
library(highcharter)
library(knitr)
library(kableExtra)
library(leaflet)

obter_penultimo_dia <- function(dados) {
  dados <- dados %>% 
    arrange(date) %>% 
    pull(date) %>% 
    unique() %>% 
    tail(2) 
  
  dados[[1]]
}

obter_ultima_data <- function(dados) {
  dados %>%
    arrange(date) %>%
    pull(date) %>%
    unique() %>%
    last()
}

formatar_data <- function(data) {
  data_split <- strsplit(as.character(data), "-")
  data_split <- data_split[[1]]
  ano <- data_split[1]
  mes <- data_split[2]
  dia <- data_split[3]
  
  data_join <- paste(dia, mes, ano, sep = "/")
  
  return(data_join)
}


loading <- function(element) {
  element %>% 
    withSpinner(type = getOption("spinner.type", default = 6), color = "#d6a9b6")
  
}
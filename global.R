# install.packages(c("tidyverse", "shiny", "tablerDash", "shinycssloaders", "zoo"))

library(tidyverse)
library(shiny)
library(shinycssloaders)
library(zoo)
library(highcharter)
library(knitr)
library(kableExtra)
library(leaflet)

# Carregando as funções --------------------------------------------------------

source("./funcoes/dash/global.R", encoding = "UTF-8")


# Carregando os módulos --------------------------------------------------------

source("./modules/home/ui.R", encoding = "UTF-8")
source("./modules/home/server.R", encoding = "UTF-8")
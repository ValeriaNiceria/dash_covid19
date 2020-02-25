source("./global.R", encoding = "utf-8")

dados_covid = obter_dados()

server <- function(input, output, session) {

  output$total_casos_confirmados <- renderUI({
    
    regiao_selecionado <- input$select_regiao
    
    dados <- dados_covid

    if (regiao_selecionado != "Todas") {
      dados <- dados %>% 
        filter(Country.Region == regiao_selecionado)
    }
    
    total_casos <- 
      dados %>% 
      group_by(data) %>% 
      summarise(total = sum(casos_confirmados)) %>% 
      dplyr::select(total) %>% 
      tail(1)
    
    tablerStatCard(
      value = total_casos$total,
      title = "Total de casos",
      width = 12
    )
    
  })
  
  
  output$total_mortes <- renderUI({
    
    regiao_selecionado <- input$select_regiao
    
    dados <- dados_covid
    
    if (regiao_selecionado != "Todas") {
      dados <- dados %>% 
        filter(Country.Region == regiao_selecionado)
    }
    
    total_mortes <-
      dados %>% 
      group_by(data) %>% 
      summarise(total = sum(mortes)) %>% 
      dplyr::select(total) %>% 
      tail(1)
    
    tablerStatCard(
      value = total_mortes$total,
      title = "Total de mortes",
      width = 12
    )
    
  })
  
  
  output$total_casos_recuperados <- renderUI({
    
    regiao_selecionado <- input$select_regiao
    
    dados <- dados_covid
    
    if (regiao_selecionado != "Todas") {
      dados <- dados %>% 
        filter(Country.Region == regiao_selecionado)
    }
    
    total_recuperados <- 
      dados %>%
      group_by(data) %>% 
      summarise(total = sum(casos_curados)) %>% 
      dplyr::select(total) %>% 
      tail(1)
    
    tablerStatCard(
      value = total_recuperados$total,
      title = "Total de casos tratados",
      width = 12
    )
    
  })
  
}
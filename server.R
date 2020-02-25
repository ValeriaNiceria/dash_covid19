server <- function(input, output, server) {
  
  output$total_casos_confirmados <- renderUI({
    
    total_casos <- 
      dados_covid %>% 
      filter(!is.na(data)) %>%
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
    
    total_mortes <-
      dados_covid %>% 
      filter(!is.na(data)) %>%
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
    
    total_recuperados <- 
      dados_covid %>% 
      filter(!is.na(data)) %>%
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
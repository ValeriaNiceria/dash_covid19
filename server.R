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
      title = "Total de recuperados",
      width = 12
    )
    
  })
  
  
  output$plot_casos_ao_longo_do_tempo <- renderHighchart({
    
    regiao_selecionado <- input$select_regiao
    
    dados <- dados_covid
    
    if (regiao_selecionado != "Todas") {
      dados <- dados %>% 
        filter(Country.Region == regiao_selecionado)
    }
    
    dados_casos <- 
      dados %>% 
      group_by(data) %>% 
      summarise(
        casos_confirmados = sum(casos_confirmados, na.rm = T),
        mortes = sum(mortes, na.rm = T),
        casos_curados = sum(casos_curados, na.rm = T)
      ) %>% 
      mutate(
        casos_confirmados1 = c(0, head(casos_confirmados, -1)),
        mortes1 = c(0, head(mortes, -1)),
        casos_curados1 = c(0, head(casos_curados, -1))
      )
    
    if (input$tipo_plot_tempo == "Real") {
      dados_casos <- 
        dados_casos %>% 
        mutate(
          casos_confirmados = casos_confirmados - casos_confirmados1,
          mortes = mortes - mortes1,
          casos_curados = casos_curados - casos_curados1
        )
    }
    
    highchart() %>% 
      hc_xAxis(categories = as.character(dados_casos$data)) %>%
      hc_add_series(
        dados_casos,
        name = "Casos",
        type = "line",
        hcaes(x = as.character(data), y = casos_confirmados)
      ) %>% 
      hc_add_series(
        dados_casos,
        name = "Mortes",
        type = "line",
        hcaes(x = as.character(data), y = mortes)
      ) %>%
      hc_add_series(
        dados_casos,
        name = "Recuperados",
        type = "line",
        hcaes(x = as.character(data), y = casos_curados)
      ) %>%
      hc_tooltip(pointFormat = paste0("{point.series.name}: <strong>{point.y}</strong>")) %>% 
      hc_colors(c("#ff9000", "#d60404", "#198c00"))
    
  })
  
  
  output$tabela_regiao <- renderUI({
    
    ultimo_dia <- obter_ultima_data(dados = dados_covid)
    
    if (input$tipo_regiao == "confirmado") {
      
      dados_regiao <-
        dados_covid %>% 
        filter(
          as.character(data) == ultimo_dia
        ) %>% 
        group_by(Country.Region) %>% 
        summarise(Total = sum(casos_confirmados, na.rm = T)) %>% 
        arrange(desc(Total))
      
    } else if (input$tipo_regiao == "morte") {
      
      dados_regiao <-
        dados_covid %>% 
        filter(
          as.character(data) == ultimo_dia
        ) %>% 
        group_by(Country.Region) %>% 
        summarise(Total = sum(mortes, na.rm = T)) %>% 
        arrange(desc(Total))
      
    } else {
      
      dados_regiao <-
        dados_covid %>% 
        filter(
          as.character(data) == ultimo_dia
        ) %>% 
        group_by(Country.Region) %>% 
        summarise(Total = sum(casos_curados, na.rm = T)) %>% 
        arrange(desc(Total))
      
    }
    
    colnames(dados_regiao)[1] <- "Região"
    
    tabela <-
    kable(dados_regiao, align = "c", row.names = FALSE, digits = 3) %>% 
      kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), font_size = 12, full_width = T) %>% 
      scroll_box( height = "350px") 
    
    HTML(tabela)
    
  })

  
  
  output$plot_mapa_regiao <- renderLeaflet({
    
    ultimo_dia <- obter_ultima_data(dados = dados_covid)
    
    if (input$tipo_regiao == "confirmado") {
      
      dados_regiao <-
        dados_covid %>% 
        filter(
          as.character(data) == ultimo_dia,
          casos_confirmados > 0
        ) %>% rename(lat = Lat, long = Long)
      
      color_bubble <- "#ff9000"
      
    } else if (input$tipo_regiao == "morte") {
      
      dados_regiao <-
        dados_covid %>% 
        filter(
          as.character(data) == ultimo_dia,
          mortes > 0
        ) %>% rename(lat = Lat, long = Long)
      
      color_bubble <- "#d60404"
      
    } else {
      
      dados_regiao <-
        dados_covid %>% 
        filter(
          as.character(data) == ultimo_dia,
          casos_curados > 0
        ) %>% rename(lat = Lat, long = Long)
      
      color_bubble <- "#198c00"
      
    }
    
    leaflet(dados_regiao) %>% 
      addTiles()  %>% 
      addProviderTiles("Esri.WorldImagery") %>%
      addCircleMarkers(~long, ~lat, 
                       fillColor = color_bubble,
                       fillOpacity = 0.7,
                       color="white",
                       radius=8, stroke=FALSE,
                       # label = mytext,
                       labelOptions = 
                         labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"),
                                       textsize = "13px", direction = "auto")
      )
    
  })
  
}
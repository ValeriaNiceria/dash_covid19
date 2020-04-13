source("./global.R", encoding = "utf-8")

server <- function(input, output, session) {
  
  dados_covid <- reactive({
    
    input_nivel <- input$input_nivel_visualizacao
    
    if (input_nivel == "Global") {
      dados <- readRDS("dados/dados_mundo.rds")
    } else {
      dados <- readRDS("dados/dados_brasil.rds")
    }
    
    
    dados
  })
  
  
  dados_periodo <- reactive({
    
    dados <- dados_covid()
    
    input_nivel_tempo <- input$tipo_plot_tempo
    penultimo_dia <- obter_penultimo_dia(dados = dados)
    ultimo_dia <- obter_ultima_data(dados = dados)
    
    
    if (input_nivel_tempo == "Novos") {
      dados <- dados %>% 
        filter(date %in% c(penultimo_dia, ultimo_dia)) %>% 
        group_by(Province.State, Country.Region, Lat,  Long) %>% 
        summarise(
          casos_confirmados = diff(casos_confirmados, na.rm = T),
          mortes = diff(mortes, na.rm = T),
          casos_curados = diff(casos_curados, na.rm = T)
        ) %>% 
        rename(lat = Lat, long = Long)
    } else {
      dados <- dados %>% 
        filter(date == ultimo_dia) %>% 
        rename(lat = Lat, long = Long)
    }
    
  })
  
  
  output$ui_body <- renderUI({
    
    input_nivel <- input$input_nivel_visualizacao
    
    if (input_nivel == "Global") {
      
      tagList(
        fluidRow(
          id="row-data",
          column(width = 7),
          column(
            width = 3,
            radioButtons(
              "tipo_plot_tempo",
              "Tipo",
              choices = c("Acumulado", "Novos"),
              inline = T
            )
          ),
          column(
            width = 2,
            span(
              id="span-data",
              "* Última atualização:",
              tags$strong(
                obter_ultima_data(dados = dados_covid()) %>% formatar_data(.)
              )
            )
          )
        ),
        uiOutput("ui_global") %>% loading()
      )
      
    } else {
      
      tagList(
        fluidRow(
          id="row-data",
          column(width = 7),
          column(
            width = 3,
            radioButtons(
              "tipo_plot_tempo_brasil",
              "Tipo",
              choices = c("Novos", "Acumulado"),
              inline = T
            )
          ),
          column(
            width = 2,
            span(
              id="span-data",
              "* Última atualização:",
              tags$strong(
                obter_ultima_data(dados = dados_covid()) %>% formatar_data(.)
              )
            )
          )
        ),
        uiOutput("ui_brasil") %>% loading()
      )
    }
    
  })
  
  
  output$ui_global <- renderUI({
    
    tagList(
      fluidRow(
        id = "row-banner",
        column(
          width = 4,
          uiOutput("total_casos_confirmados_global") %>% loading()
        ),
        column(
          width = 4,
          uiOutput("total_mortes_global") %>% loading()
        ),
        column(
          width = 4,
          uiOutput("total_casos_recuperados_global") %>% loading()
        )
      ),
      
      fluidRow(
        tablerCard(
          width = 12,
          title = "Top regiões - Distribuição no mundo",
          radioButtons(
            "input_tipo",
            "Tipo",
            choices = c("Casos confirmados" = "confirmado",
                        "Mortes" = "morte",
                        "Casos recuperados" = "recuperado"),
            inline = T
          ),
          fluidRow(
            column(
              width = 3,
              uiOutput("tabela_regiao_global") %>% loading()
            ),
            
            column(
              width = 9,
              leafletOutput('plot_mapa_regiao_global', height = "400px") %>% loading()
            )
          )
        )
      ),
      
      fluidRow(
        tablerCard(
          width = 12,
          title = "Coronavírus (COVID-19) ao longo do tempo",
          fluidRow(
            style = "margin-top: -66px;",
            column(width = 9),
            column(
              width = 3,
              selectInput(
                "input_select_regiao",
                "Região:",
                choices = c("Geral" = "geral", dados_periodo() %>% 
                              filter(!is.na(Country.Region)) %>% 
                              ungroup() %>% 
                              mutate(Country.Region = as.character(Country.Region)) %>%
                              pull(Country.Region) %>% unique())
              )
            )
          ),
          highchartOutput("plot_casos_ao_longo_do_tempo_global") %>% loading()
        )
      )
    )
    
  })
  
  
  output$ui_brasil <- renderUI({
    
    # tagList(
    #   
    #   fluidRow(
    #     id = "row-banner",
    #     column(
    #       width = 4,
    #       uiOutput("total_casos_confirmados_brasil") %>% loading()
    #     ),
    #     column(
    #       width = 4,
    #       uiOutput("total_mortes_brasil") %>% loading()
    #     ),
    #     column(
    #       width = 4,
    #       uiOutput("total_casos_recuperados_brasil") %>% loading()
    #     )
    #   )
    #   
    # )
    
  })
  
  
  # Begin - Global ----

  output$total_casos_confirmados_global <- renderUI({
    
    dados <- dados_periodo()
    
    total_casos <- dados %>% pull(casos_confirmados) %>% sum(., na.rm = T)
    total_formatado <- format(as.numeric(total_casos), big.mark=".")
    
    tablerStatCard(
      value = total_formatado,
      title = "Total de casos",
      width = 12
    )
    
  })
  
  
  output$total_mortes_global <- renderUI({
    
    dados <- dados_periodo()
    
    total_mortes <- dados %>% pull(mortes) %>% sum(., na.rm = T)
    total_formatado <- format(as.numeric(total_mortes), big.mark=".")
    
    tablerStatCard(
      value = total_formatado,
      title = "Total de mortes",
      width = 12
    )
    
  })
  
  
  output$total_casos_recuperados_global <- renderUI({
    
    dados <- dados_periodo()
    
    total_curados <- dados %>% pull(casos_curados) %>% sum(., na.rm = T)
    total_formatado <- format(as.numeric(total_curados), big.mark=".")
    
    tablerStatCard(
      value = total_formatado,
      title = "Total de recuperados",
      width = 12
    )
    
  })
  
  
  output$tabela_regiao_global <- renderUI({
    
    dados <- dados_periodo()
    
    input_tipo <- input$input_tipo
    
    if (input_tipo == "confirmado") {
      dados_regiao <-
        dados %>% 
        group_by(Country.Region) %>% 
        summarise(Total = sum(casos_confirmados, na.rm = T)) %>% 
        arrange(desc(Total))
    } else if (input_tipo == "morte") {
      dados_regiao <-
        dados %>%  
        group_by(Country.Region) %>% 
        summarise(Total = sum(mortes, na.rm = T)) %>% 
        arrange(desc(Total))
    } else {
      dados_regiao <-
        dados %>% 
        group_by(Country.Region) %>% 
        summarise(Total = sum(casos_curados, na.rm = T)) %>% 
        arrange(desc(Total))
    }
    
    dados_regiao <- dados_regiao %>% 
      mutate(
        `%` = round((Total/sum(Total))*100, 1)
      )
    
    colnames(dados_regiao)[1] <- "Região"
    
    tabela <-
    kable(dados_regiao, align = "c", row.names = FALSE, digits = 3) %>% 
      kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), font_size = 12, full_width = T) %>% 
      column_spec(3, bold = TRUE, include_thead =TRUE) %>% 
      scroll_box( height = "399px") 
    
    HTML(tabela)
    
  })

  
  
  output$plot_mapa_regiao_global <- renderLeaflet({
    
    dados <- dados_periodo()
    
    input_tipo <- input$input_tipo
    
    if (input_tipo == "confirmado") {
      
      dados_regiao <-
        dados %>% 
        filter(casos_confirmados > 0)
      
      color_bubble <- "#ff9000"
      
      mytext <- paste0(
        "<b>País ou região:</b> ", dados_regiao$Country.Region, "<br/>",
        "<b>Província ou estado:</b> ", ifelse(dados_regiao$Province.State == "", "Sem informação", as.character(dados_regiao$Province.State)), "<br/>", 
        "<b>Total de casos confirmados:</b> ", dados_regiao$casos_confirmados, "<br/>"
        ) %>%
        lapply(htmltools::HTML)
      
    } else if (input_tipo == "morte") {
      
      dados_regiao <-
        dados %>% 
        filter(mortes > 0)
      
      color_bubble <- "#d60404"
      
      mytext <- paste0(
        "<b>País ou região:</b> ", dados_regiao$Country.Region, "<br/>",
        "<b>Província ou estado:</b> ", ifelse(dados_regiao$Province.State == "", "Sem informação", as.character(dados_regiao$Province.State)), "<br/>", 
        "<b>Total de mortes:</b> ", dados_regiao$mortes, "<br/>"
      ) %>%
        lapply(htmltools::HTML)
      
    } else {
      
      dados_regiao <-
        dados %>% 
        filter(casos_curados > 0)
      
      color_bubble <- "#198c00"
      
      mytext <- paste0(
        "<b>País ou região:</b> ", dados_regiao$Country.Region, "<br/>",
        "<b>Província ou estado:</b> ", ifelse(dados_regiao$Province.State == "", "Sem informação", as.character(dados_regiao$Province.State)), "<br/>", 
        "<b>Total de recuperados:</b> ", dados_regiao$casos_curados, "<br/>"
      ) %>%
        lapply(htmltools::HTML)
      
    }
    
    leaflet(dados_regiao) %>% 
      addTiles()  %>% 
      addProviderTiles("Esri.WorldImagery") %>%
      addCircleMarkers(~long, ~lat, 
                       fillColor = color_bubble,
                       fillOpacity = 0.7,
                       color="white",
                       radius=8, stroke=FALSE,
                       label = mytext,
                       labelOptions = 
                         labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"),
                                       textsize = "13px", direction = "auto")
      )
    
  })
  
  
  output$plot_casos_ao_longo_do_tempo_global <- renderHighchart({
    
    dados <- dados_covid()
    
    input_nivel_tempo <- input$tipo_plot_tempo
    input_select_regiao <- input$input_select_regiao
    
    
    if (input_select_regiao != "geral") {
      
      dados <- dados %>% 
        filter(Country.Region == input_select_regiao)
    }
      
    dados <- 
      dados %>% 
      group_by(date) %>% 
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
      
    if (input_nivel_tempo == "Novos") {
      dados <- 
        dados %>% 
        mutate(
          casos_confirmados = casos_confirmados - casos_confirmados1,
          mortes = mortes - mortes1,
          casos_curados = casos_curados - casos_curados1
        )
    }
      
      
    highchart() %>%
      hc_xAxis(categories = as.character(dados$date)) %>%
      hc_add_series(
        dados,
        name = "Casos",
        type = "line",
        hcaes(x = as.character(date), y = casos_confirmados)
      ) %>%
      hc_add_series(
        dados,
        name = "Mortes",
        type = "line",
        hcaes(x = as.character(date), y = mortes)
      ) %>%
      hc_add_series(
        dados,
        name = "Recuperados",
        type = "line",
        hcaes(x = as.character(date), y = casos_curados)
      ) %>%
      hc_tooltip(pointFormat = paste0("{point.series.name}: <strong>{point.y}</strong>")) %>%
      hc_colors(c("#ff9000", "#d60404", "#198c00"))
  })
  
  
  # Begin - Brasil ----
  
  
  output$total_casos_confirmados_brasil <- renderUI({
    
    dados <- dados_covid()
    
    input_nivel_tempo <- input$tipo_plot_tempo_brasil
    ultimo_dia <- obter_ultima_data(dados)
    
    if (input_nivel_tempo == "Novos") {
      dados <- dados %>% 
        filter(date == ultimo_dia)
    }
    
    dados <- dados %>% pull(confirmed) %>% sum(., na.rm = T)
    
    total_formatado <- format(as.numeric(dados), big.mark=".")
    
    tablerStatCard(
      value = total_formatado,
      title = "Total de casos",
      width = 12
    )
    
  })
  
  
  output$total_mortes_brasil <- renderUI({
    # 
    # dados <- dados_covid()
    # 
    # total_mortes <- dados %>% pull(mortes) %>% sum(., na.rm = T)
    # total_formatado <- format(as.numeric(total_mortes), big.mark=".")
    # 
    # tablerStatCard(
    #   value = total_formatado,
    #   title = "Total de mortes",
    #   width = 12
    # )
    # 
  })
  
  
  output$total_casos_recuperados_brasil <- renderUI({
    
    # dados <- dados_covid()
    # 
    # total_curados <- dados %>% pull(casos_curados) %>% sum(., na.rm = T)
    # total_formatado <- format(as.numeric(total_curados), big.mark=".")
    # 
    # tablerStatCard(
    #   value = total_formatado,
    #   title = "Total de recuperados",
    #   width = 12
    # )
    
  })
  
  
}
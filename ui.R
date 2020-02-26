source("./global.R", encoding = "utf-8")

dados_covid = obter_dados()

ui <- tags$html(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "css/style.css"),
    tags$script(HTML('
         $(document).ready(function() {
           $(".header-brand").attr("href","https://valerianiceria.shinyapps.io/dash_covid19")
           /* ICONE */
           $(".fe").addClass("fa").removeClass("fe");
           $(".fe-chevron-up").addClass("fa-chevron-up").removeClass("fe-chevron-up");
           $(".fe-database").addClass("fa-database").removeClass("fe-database");
           $(".fe-maximize").addClass("fa-window-maximize").removeClass("fe-maximize");
           $(".fe-bar-chart").addClass("fa-bar-chart").removeClass("fe-bar-chart");
           $(".fe-x").addClass("fa-close").removeClass("fe-x");
        })
    '))
  ),
  
  tags$body(
    
    tablerDashPage(
      navbar = tablerDashNav(
        id = "menuCovid",
        src = "img/logo.png",
        selectInput(
          "select_regiao",
          "Selecione uma região:",
          choices = c("Todas", 
                      as.character(
                        dados_covid %>%
                          arrange(Country.Region) %>%
                          pull(Country.Region)
                        )
                      )
        )
      ),

      title = "COVID-19",
      
      body = tablerDashBody(
        
        tablerTabItems(
          
          tablerTabItem(
            
            tabName = "home",

            fluidRow(
              id="row-data",
              column(
                width = 12,
                span(
                  id="span-data",
                  paste(
                    "* Dados do dia:",
                    obter_ultima_data(dados = dados_covid)
                  )
                )
              )
            ),
            
            fluidRow(
              id = "row-banner",
              column(
                width = 3,
                uiOutput("total_casos_confirmados")
              ),
              column(
                width = 3,
                uiOutput("total_mortes")
              ),
              column(
                width = 3,
                uiOutput("total_casos_recuperados")
              )
            ),
            
            fluidRow(
              column(
                width = 12,
                tablerCard(
                  width = 12,
                  title = "Coronávirus (COVID-19) ao longo do tempo",
                  highchartOutput("plot_casos_ao_longo_do_tempo") %>% withSpinner() 
                )
              )
            )
            
          )
          
        )
      ),
      
      footer = tablerDashFooter()
      
    )
  )
)
source("./global.R", encoding = "utf-8")

dados_covid = obter_dados()

ui <- tags$html(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "css/style.css"),
    tags$link(rel="icon", href="image/favicon.png", type="image/x-icon"),
    tags$script(HTML('
         $(document).ready(function() {
         $(".header-brand").attr("href","https://valerianiceria.shinyapps.io/dash_covid19")
         /* ICONE */
         $(".fe").addClass("fa").removeClass("fe");
         $(".fe-database").addClass("fa-database").removeClass("fe-database");
         $(".fe-bar-chart").addClass("fa-bar-chart").removeClass("fe-bar-chart");
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
        ),
        selectInput(
          "select_mes",
          "Selecione um mês:",
          choices = c("Todos", 
                      as.character(
                        dados_covid %>%
                          filter(!is.na(Mes)) %>% 
                          arrange(Mes) %>%
                          pull(Mes) %>% 
                          unique()
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
            )
            
          )
          
        )
      ),
      
      footer = tablerDashFooter()
      
    )
  )
)
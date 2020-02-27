source("./global.R", encoding = "utf-8")

dados_covid = obter_dados()

ui <- tags$html(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "css/style.css"),
    tags$script(HTML('
         $(document).ready(function() {
           $(".header-brand").attr("href","https://valerianiceria.shinyapps.io/dash_covid19")
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
                width = 4,
                uiOutput("total_casos_confirmados")
              ),
              column(
                width = 4,
                uiOutput("total_mortes")
              ),
              column(
                width = 4,
                uiOutput("total_casos_recuperados")
              )
            ),
            
            fluidRow(
              column(
                width = 12,
                tablerCard(
                  width = 12,
                  title = "Coronavírus (COVID-19) ao longo do tempo",
                  radioButtons(
                    "tipo_plot_tempo",
                    "Tipo",
                    choices = c("Acumulado", "Real"),
                    inline = T
                  ),
                  highchartOutput("plot_casos_ao_longo_do_tempo") %>% withSpinner() 
                )
              )
            ),
            
            fluidRow(
              column(
                width = 12,
                tablerCard(
                  width = 12,
                  title = "Top regiões e distribuição no mundo",
                  radioButtons(
                    "tipo_regiao",
                    "Tipo",
                    choices = c("Casos confirmados" = "confirmado",
                                "Mortes" = "morte",
                                "Casos recuperados" = "recuperado"),
                    inline = T
                  ),
                  column(
                    width = 3,
                    uiOutput("tabela_regiao")
                  ),
                  
                  column(
                    width = 9,
                    leafletOutput('plot_mapa_regiao', height = "400px") %>% withSpinner()
                  )
                )
              )
  
            )
            
          )
          
        )
      )
    ),
    shiny::tags$footer(
      shiny::tags$div(
        class="container",
        shiny::tags$div(
          class="row align-items-center flex-row-reverse",
          shiny::tags$div(
            class="col-auto ml-lg-auto",
            shiny::tags$div(
              class="row align-items-center",
              shiny::tags$div(
                class="col-auto",
                shiny::tags$a(
                  href="https://github.com/ValeriaNiceria/dash_covid19",
                  class="btn btn-outline-primary btn-sm",
                  "Código",
                  target="_blank",
                  alt = "valeria niceria"
                )
              )
            )
          ),
          shiny::tags$div(
            class="col-12 col-lg-auto mt-3 mt-lg-0 text-center",
            "Copyright © 2020 - desenvolvido por Valéria Nicéria"
          )
        )
      )
    )
  )
)
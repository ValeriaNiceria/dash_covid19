source("./global.R", encoding = "utf-8")

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
        radioButtons(
          "input_nivel_visualizacao",
          "Nível",
          choices = c("Global", "Brasil"),
          inline = T
        )
      ),

      title = "COVID-19",
      body = tablerDashBody(
        tablerTabItems(
          tablerTabItem(
            tabName = "home",
            uiOutput("ui_body") %>% loading()
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
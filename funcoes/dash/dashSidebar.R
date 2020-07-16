
dashSidebar <- function() {
  
  shiny::tags$div(
    class="sidebar",
    shiny::tags$div(
      class="sidebar-wrapper",
      shiny::tags$div(
        class="logo",
        shiny::tags$a(
          class="simple-text logo-mini",
          href="javascript:void(0)",
          icon("chart-pie")
        ),
        shiny::tags$a(
          class="simple-text logo-normal",
          href="javascript:void(0)",
          "COVID-19"
        )
      ),
      
      tags$ul(
        class="nav",
        tags$li(
          class="active",
          tags$a(
            href="#",
            shiny::icon("home"),
            tags$p(
              "Dashboard"
            )
          )
        )
        
      )
    )
  )
  
}
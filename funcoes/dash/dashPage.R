
dashPage <- function(
  title = NULL,
  header = NULL,
  sidebar = NULL,
  navbar = NULL,
  body = NULL,
  footer = NULL
) {
  
  
  shiny::tags$html(
    
    # Head ---------------------------------------------------------------------
    shiny::tags$head(
      shiny::tags$meta(
        charset="utf-8"
      ),
      shiny::tags$meta(
        `http-equiv`="X-UA-Compatible",
        content="IE=edge"
      ),
      shiny::tags$meta(
        name="viewport",
        content="width=device-width, initial-scale=1, shrink-to-fit=no"
      ),
      tags$link(
        rel="stylesheet",
        href="https://fonts.googleapis.com/css?family=Poppins:200,300,400,600,700,800"
      ),
      tags$link(
        rel="stylesheet",
        href="./css/fontawesome.css"
      ),
      tags$link(
        rel="stylesheet",
        href="./dash/css/nucleo-icons.css"
      ),
      tags$link(
        rel="stylesheet",
        href="./dash/css/black-dashboard.css?v=1.0.0"
      ),
      tags$link(
        rel="stylesheet",
        href="./dash/demo/demo.css"
      ),
      shiny::tags$title(
        title
      )
    ),
    
    # Body ---------------------------------------------------------------------
    
    shiny::tags$body(
      class="",
      shiny::tags$div(
        class="wrapper",
        
        # Sidebar --------------------------------------------------------------
        sidebar,
        
        # Conteúdo - Body ------------------------------------------------------
        shiny::tags$div(
          class="content-wrapper",
          
          # Header -------------------------------------------------------------
          header,

          body
        ),
        
        footer
        
      )
    ),
    
    # Footer script ------------------------------------------------------------
    htmltools::htmlDependency("jquery", "3.4.1",
                              src = c(href = "https://code.jquery.com/"),
                              script = "jquery-3.4.1.min.js"),
    htmltools::htmlDependency("jquery-ui", "1.12.1",
                              src = c(href = "https://code.jquery.com/ui/1.12.1/"),
                              script = "jquery-ui.min.js"),
    
    tags$script(
      src="./dash/js/core/jquery.min.js"
    ),
    tags$script(
      src="./dash/js/core/popper.min.js"
    ),
    tags$script(
      src="./dash/js/core/bootstrap.min.js"
    ),
    tags$script(
      src="./dash/js/plugins/perfect-scrollbar.jquery.min.js"
    ),
    tags$script(
      src="./dash/js/plugins/chartjs.min.js"
    ),
    tags$script(
      src="./dash/js/plugins/bootstrap-notify.js"
    ),
    tags$script(
      src="./dash/js/black-dashboard.min.js?v=1.0.0"
    ),
    tags$script(
      src="./dash/demo/demo.js"
    )
  )
  
  
  
  
}
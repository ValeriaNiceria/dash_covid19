
source("./global.R", encoding = "UTF-8")

ui <- dashPage(
  title = "COVID-19",
  sidebar = dashSidebar()
)

server <- function(input, output, session) {
  
}


shinyApp(ui, server)
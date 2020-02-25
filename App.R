source("./global.R", encoding = "utf-8")
source("./ui.R", encoding = "utf-8")
source("./server.R", encoding = "utf-8")

theme_set(theme_ipsum(base_size = 10))

dados_covid = obter_dados()

shinyApp(ui, server)
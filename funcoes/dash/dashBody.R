
dashBody <- function(
  header = NULL
) {
  
  tags$div(
    class="main-panel",
    header,
    
    tags$div(
      class="content"
    )
    
  )
  
}
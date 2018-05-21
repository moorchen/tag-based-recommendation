library(shiny)

shinyUI(fluidPage(
  
  theme = "shiny.css",
  
  titlePanel("Tag-based Recommandation System"),
  
  sidebarLayout(
    ###################################################
    sidebarPanel(
      numericInput("id",
                   "movieId:",
                   min = 1,
                   max = 131262,
                   value = 1),
      numericInput("number",
                   "recommandation number:",
                   min = 1,
                   max = 50,
                   value = 8),
      textInput("search", "Search"),
      tableOutput("candidates")
    ),
    ###################################################
    mainPanel(
      h3("Input Information"),
      fluidRow(
        htmlOutput("input")
      ),
      h3("Movies you may like"),
      fluidRow(
        htmlOutput("test")
      )
    )
    ###################################################
  )
  
))

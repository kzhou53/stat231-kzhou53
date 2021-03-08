library(shiny)

ui <- fluidPage(
  sliderInput(inputId = "num", 
              label = "Choose a number", 
              value = 25, min = 1, max = 100),
  textInput(inputId = "title", 
            label = "Write a title",
            value = "Histogram of Random Normal Values"),
  plotOutput("hist"),
  verbatimTextOutput("stats")
)

server <- function(input, output) {
  output$hist <- renderPlot({
    hist(rnorm(input$num), main = input$title)
  })
  output$stats <- renderPrint({
    summary(rnorm(input$num))
  })
}

shinyApp(ui = ui, server = server)

#rv <- reactiveValues(
 # norm = rnorm(25), # wanted to do norm = rnorm(input$num) but didnt work
  #unif = runif(25), # see above
  #chisq = rchisq(25, 2)) # see above

#observeEvent(input$renorm, { 
 # rv$norm <- rnorm(input$num) })
#observeEvent(input$reunif, { 
  #rv$unif <- runif(input$num) })
#observeEvent(input$rechisq, { 
  #rv$chisq <- rchisq(input$num, 2) })
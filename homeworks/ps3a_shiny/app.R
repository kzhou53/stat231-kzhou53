library(shiny)

ui <- fluidPage(
  sliderInput(inputId = "num", 
              label = "Choose a number", 
              value = 25, min = 1, max = 100),
  textInput(inputId = "title", 
            label = "Write a title",
            value = "Histogram of Random Normal Values"),
  navlistPanel(              
    tabPanel(title = "Normal data",
             plotOutput("norm"),
             actionButton("renorm", "Resample")
    ),
    tabPanel(title = "Uniform data",
             plotOutput("unif"),
             actionButton("reunif", "Resample")
    ),
    tabPanel(title = "Chi Squared data",
             plotOutput("chisq"),
             actionButton("rechisq", "Resample")
    )
  )
)

server <- function(input, output) {
  rv <- reactiveValues(
    norm = rnorm(25), 
    unif = runif(25),
    chisq = rchisq(25, 2))
  
  observeEvent(input$renorm, { rv$norm <- rnorm(input$num) })
  observeEvent(input$reunif, { rv$unif <- runif(input$num) })
  observeEvent(input$rechisq, { rv$chisq <- rchisq(input$num, 2) })
  
  output$norm <- renderPlot({
    hist(rv$norm(input$num), main = isolate({input$title}))
  })
  output$stats <- renderPrint({
    summary(rnorm(input$num))
  })
  output$unif <- renderPlot({
    hist(rv$unif, main = isolate({input$title}))
  })
  output$chisq <- renderPlot({
    hist(rv$chisq, main = isolate({input$title}))
  })  
}

shinyApp(ui = ui, server = server)
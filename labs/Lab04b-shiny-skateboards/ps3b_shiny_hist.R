# this code reproduces the histogram panel only from the electric skateboards app
library(shiny)
library(tidyverse)


###############
# import data #
###############
skateboards <- read_csv("electric_skateboards.txt")

###################################################
# define choice values and labels for user inputs #
###################################################
# define vectors for choice values and labels 
# can then refer to them in server as well (not just in defining widgets)
# for selectInput, 'choices' object should be a NAMED LIST
hist_choice_values <- c("price","range","top_speed","weight","battery")
hist_choice_names <- c("Price","Range","Top Speed","Weight","Battery")
names(hist_choice_values) <- hist_choice_names

# for checkboxGroupInput
drv_choices <-  unique(skateboards$drive)


############
#    ui    #
############
ui <- navbarPage(
  
  title="Electric Skateboards",
  
  tabPanel(
    title = "Histogram",
    sidebarLayout(
      sidebarPanel(
        checkboxGroupInput(inputId = "drv"
                           , label = "Include drive types:"
                           , choices = drv_choices
                           , selected = drv_choices
                           , inline = TRUE),
        sliderInput(inputId = "interval"
                    , label = "Choose the maximum price you want plotted:"
                    , value = 3000
                    , min = 180
                    , max = 3700)
      ),
      mainPanel(
        plotOutput(outputId = "hist")
      )
    )
  )
)

############
# server   #
############
server <- function(input,output){
  data_for_hist <- reactive({
    data <- filter(skateboards, drive %in% input$drv)
  })
  

  # TAB 1: HISTOGRAM
  output$hist <- renderPlot({
    ggplot(data = data_for_hist(), aes(x = price)) +
      geom_histogram(color = "#2c7fb8", fill = "#7fcdbb", alpha = 0.7) +
      xlim(180, input$interval) +
      labs(x = "Price"
          , y = "Number of Skateboards")
  })
  
}

####################
# call to shinyApp #
####################
shinyApp(ui = ui, server = server)
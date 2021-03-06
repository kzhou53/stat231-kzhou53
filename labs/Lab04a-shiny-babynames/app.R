# Q: "Is there a way to change the name(s) being illustrated, 
#      either in the code or through a more interactive plot?"
# A: Yes!
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

# data
library(babynames)
babynames_dat <- babynames::babynames

# create choices vector for name choices
# too many names! app will be sloooooow
#name_choices <- babynames_dat %>% 
#  count(name) %>%
#  select(name)

# subset on names of students in this course so doesn't crash ...
name_choices <- str_to_title(c("katharine", "andrea"
                            # section 1
                            , "amaya", "ava", "alastair", "clara", "daniel"
                            , "ethan", "evan", "jamie", "joshua", "kim"
                            , "kriti", "masahiro", "matthew", "sanjana"
                            , "sarah", "seamus", "siyi", "stefan", "vaibhav"
                            , "willie" 
                            # section 2
                            , "alex", "alison", "ayodele", "brandon"
                            , "caroline", "clara", "conrad", "emma", "eric"
                            , "graham", "jett", "kevin", "lorraine", "lovemore"
                            , "maggie", "sabrina", "sammy", "sophie", "teddy"
                            , "zac"))
name_choices


# Define UI for application that creates a line plot for a given name
ui <- fluidPage(
   
   # Application title
   titlePanel("The Number of Babies with a Specified Name and Gender"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        # Select Name
        selectInput(inputId = "nm", 
                    label = "Name:",
                    choices = name_choices, 
                    selected = "Kim"),
        # Choose Sex
        radioButtons(inputId = "sx", 
                    label = "Sex:",
                    choices = c("M", "F"), 
                    selected = "F")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
     
     dat <- babynames_dat %>%
       filter(name %in% input$nm & sex == input$sx) %>%
       group_by(name, year) %>%
       summarize(total = sum(n))
     
     ggplot(data = dat, aes(x = year, y = total)) +
       geom_line(color = "#0095b6") + 
       labs(x = "Year", y = "Total number of births with this name"
            , title = paste("Babies Named", input$nm))
     
   })
}


# Run the application 
shinyApp(ui = ui, server = server)

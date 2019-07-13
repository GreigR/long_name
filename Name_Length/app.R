

library(shiny)
library(shinydashboard)
library(tidyverse)
library(glue)

Freq_table <-
    read_csv("/home/greig/R-projects/Long_name/Freq_table.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(# Application title
    titlePanel("Last name length analysis"),
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput(
                "length",
                "Last name length",
                min = 1,
                max = 27,
                value = 20
            )
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("freq_plot"),
            textOutput("results_1"),
            textOutput("results_2")
        )
    ))

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$freq_plot <- renderPlot({
        ggplot(Freq_table, aes(Count_letters, Freq)) +
            geom_col() +
            geom_vline(
                xintercept = input$length,
                color = "red",
                size = 2
            )
        
    })
    
    output$results_1 <- renderText({
        Max_char <- 27
        Denom <- 76874
        Key <- input$length
        
        Num <- 0
        for (i in Key:Max_char) {
            Num = Num + Freq_count$Freq[i]
        }
        
        Lost <- round(((Num / Denom) * 100), digits = 2)
        
        glue(
            'if {input$length} characters are allocated to the last name then {Lost} % of the labels will have missing letters in the last name but'
        )
    })
    output$results_2 <- renderText({
        Key2 <- input$length - 10
        
        Num <- 0
        for (i in 1:Key2) {
            Num = Num + Freq_count$Freq[i]
        }
        
        White <- round(((Num / Denom) * 100), digits = 2)
        
        glue('{White}% of labels will have more than 10 unused characters')
    })
}

# Run the application
shinyApp(ui = ui, server = server)


library(shiny)
library(tidyverse)

Freq_table <- read_csv("/home/greig/R-projects/Long_name/Freq_table.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Last name length analysis"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("length",
                        "Last name length",
                        min = 1,
                        max = 27,
                        value = 20)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("freq_plot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$freq_plot <- renderPlot({
        ggplot(Freq_table, aes(Count_letters, Freq)) +
            geom_col() +
            geom_vline(xintercept = input$length, color = "red", size = 2)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

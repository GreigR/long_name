
library(shiny)
library(tidyverse)
library(glue)

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
           plotOutput("freq_plot"),
           textOutput("results")
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
    
    output$results <- renderText({
        Max_char <- 27
        Denom <- 76874
        Key <- input$length
        
        Num <- 0
        for (i in Key:Max_char) {
            Num = Num + Freq_count$Freq[i]
        }
        
        Lost <- round(((Num/Denom)*100), digits = 2)
        
        Key2 <- Key - 10
        
        Num <- 0
        for (i in 1:Key2) {
            Num = Num + Freq_count$Freq[i]
        }
        
        White <- round(((Num/Denom)*100), digits = 2)
        
        glue('With {input$length} characters for the last name, {Lost} % of the labels will have missing letters and {White}% of labels will have excessive white space')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

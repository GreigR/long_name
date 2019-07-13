#Shiny dashboard for exploring the impact of name length

library(shiny)
library(shinydashboard)
library(tidyverse)

#load the data
Freq_table <- read_csv("Freq_table.csv")

#Shiny dashboard

my_Header <- dashboardHeader(title = "Label explorer",
                             titleWidth = 300)

my_Sidebar <- dashboardSidebar(
    wellPanel(
        sliderInput(
            inputId = "length",
            label = "Last name length",
            min = 1,
            max = 27,
            value = 20
        )
    )
)

my_Body <- dashboardBody(
    tabItems(
        tabItem(
            fluidPage(
                fluidRow(
                    box(
                        width = 12,
                        column(
                            12,
                            align = "center",
                            plotOutput("freq_plot"),
                            align = "right"
                        )
                    )
                ),
                fluidRow(
                    valueBox(
                        value = input$length,
                        subtitle = "Number of characters for last name",
                        color = "green"
                    ),
                    valueBox(
                        value = Lost,
                        subtitle = "% incomplete names",
                        color = "green"
                    ),
                    valueBox(value = White,
                             subtitle = "% more than 10 unused characters")
                )
                )
            )
        )
    )


#Define UI for application

ui <- dashboardPage(
    skin = "green",
    header = my_Header,
    sidebar = my_Sidebar,
    body = my_Body
)

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

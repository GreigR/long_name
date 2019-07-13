

library(shiny)
library(shinydashboard)
library(tidyverse)

Freq_count <- read_csv("Freq_table.csv")
Key <- 20
Max_char <- 27
Denom <- 76874


my_Header <- dashboardHeader(title = "Labels explorer",
                             titleWidth = 300)

my_Sidebar <- dashboardSidebar(
        sliderInput(
            inputId = "length",
            label = "Last name length",
            min = 1,
            max = 27,
            value = 20
        )
)

my_Body <- dashboardBody(
    wellPanel(fluidRow(
        h1("Space is tight on the patient labels especially about the last name"),
        h2("Getting the balance right is important so how much space should be given to the last name?")
    )),
    fluidRow(
        plotOutput("freq_plot")
    ),
    wellPanel(fluidRow(
        valueBoxOutput("box_length"),
        valueBoxOutput("box_lost"),
        valueBoxOutput("box_white")
    ))
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
        ggplot(Freq_count, aes(Count_letters, Freq)) +
            geom_col() +
            geom_vline(
                xintercept = input$length,
                color = "red",
                size = 2
            ) +
            labs(x = "Number of letters in the patient's last name", y = "Frequency of occurance") +
            theme_light()
        
    })
    
    output$box_length <- renderValueBox(
        valueBox(
            value = input$length,
            subtitle = "If this number  of characters is allocated for last name then",
            color = "green"
        )
    )
    
    output$box_lost <- renderValueBox({
        Key <- input$length
        
        Num <- 0
        for (i in Key:Max_char) {
            Num = Num + Freq_count$Freq[i]
        }
        
        Lost <- round(((Num / Denom) * 100), digits = 2)
        
        valueBox(
            value = Lost,
                 subtitle = "% of patients will have labels with incomplete last names",
                 color = "green")
    })
    
    output$box_white <- renderValueBox({
        Key2 <- input$length - 10
        
        Num <- 0
        for (i in 1:Key2) {
            Num = Num + Freq_count$Freq[i]
        }
        
        White <- round(((Num / Denom) * 100), digits = 2)
        
        valueBox(
            value = White,
            subtitle = "% of labels will have the space for a phone number unused",
            color = "green")
    })
    
}
# Run the application 
shinyApp(ui = ui, server = server)

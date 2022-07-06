library(shiny)

model_rate <- readRDS("model_forest_rate.rds")
model_rate_activism <- readRDS("model_forest_rate_activism.rds")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("educ", label = "Welches ist die höchste Ausbildung, die Sie mit einem Zeugnis oder Diplom abgeschlossen haben?",
                            choices = c("Grundausbildung (inklusive nicht abgeschlossen)",
                                        "Erstausbildung",
                                        "Zweitausbildung"))
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
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

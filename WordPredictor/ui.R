library(shiny)

shinyUI(fluidPage(

    # Application title
    titlePanel("Word Predictor:"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            textInput("phrase", "Enter phrase here:", value = "",placeholder = "Enter here")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            verbatimTextOutput("phrase"),
            br(),
            verbatimTextOutput("pred1"),
            verbatimTextOutput("pred2"),
            verbatimTextOutput("pred3")
        )
    )
))

library(shiny)

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    
    # Application title
    titlePanel("Word Predictor Demo"),
    
    fluidRow(
        
        column(8,
               wellPanel(
                   textInput("inputText", label = h3("Input text"), 
                             value = "")
               )       
        ),
        column(8,
               h3("Predicted words"),
               tableOutput("wordPredictions")
        )
    )
))
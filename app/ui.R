#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Text Prediction"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            h6("This application uses n-gram NLP modelling on a Swiftkey provided text corpus to predict
               the next word that you might want to type. Here we choose to use the 'News' corpus to predict."),
            br(),
            h5("Please give about 10 seconds to start up the prediction engine! Once you see output appearing you
               can type in real time for predictions. Thank you!"),
            br(),
            textAreaInput("current", "Type here:", "type something here to receive predictions in real time")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            textOutput("value"),
            br(),
            h6("Top 3 predictions:"),
            textOutput("suggestions")
        )
    )
))

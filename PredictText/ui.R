#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

fluidPage(
    titlePanel("Predict next words for your text input"),
    sidebarLayout(
        sidebarPanel(
            textInput(inputId = "textInput",
                      label = "Your text input",
                      value = "Write your text here. Enjoy!"),
            sliderInput(inputId = "ngramInput",
                         label = "How many words you want to predict",
                         value = 2,
                         min = 1,
                         max = 4)
        ),
        mainPanel(
            p(h3("Your text will be followed by:"),
            textOutput("textOutput")),  
            p(h3("Phrase and their importance by TF-IDF"),
              tableOutput("tableOutput")),
            p(h3("Top 5 most important word chunks"),
              plotOutput("plotOutput"))
        )
    )
)
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
    titlePanel("Superficies Adaptativas"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("seed",
                        "Índice da simulação:",
                        min = 0,
                        max = 100,
                        step = 1,
                        value = 42)
            ,
            sliderInput("corr_1",
                        "Correlação genética da população 1:",
                        min = -1,
                        max = 1,
                        step = 0.05,
                        value = 0.8)
            ,
            sliderInput("corr_2",
                        "Correlação genética da população 2:",
                        min = -1,
                        max = 1,
                        step = 0.05,
                        value = 0.0)
            ,
            sliderInput("n_peaks",
                        "Número de picos adaptativos:",
                        min = 1,
                        max = 10,
                        step = 1,
                        value = 1)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distPlot")
        )
    )
))

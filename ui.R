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
    titlePanel("Wilmington Arrests by Census Tract 2010-2018"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("year", label = "Year of Arrest",
                        min = 2010, max = 2018, value = 2010, sep = "", animate = animationOptions(interval = 500, loop = TRUE)),
            selectInput("data",label = "Demographic", c("Total Arrests", "Black Arrests", "White Arrests")),
            selectInput("stat", label = "Statistic", c("None", "Percent Population Arrested","Percent of Total Arrests", "SIR","Poisson Regression"))
        ),

        # Show a plot of the generated distribution
        mainPanel(
            textOutput("text"),
            plotOutput("map"),
            tableOutput("table")
        )
    )
))

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
    titlePanel("Callodine Capital Master Fund Intra-Day Performance"),

    # Show a plot of the generated distribution
    fluidRow(
        column(6, align="center", plotOutput("Plot")),
        column(6, align="center", gt_output("TableLS"))
    )


))

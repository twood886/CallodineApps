#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)

# Define UI for application that draws a histogram
shinyUI(

  dashboardPage(
    dashboardHeader(title = "Callodine Capital Master Fund Intra-Day Performance"),
    dashboardSidebar(),
    dashboardBody(
      fluidRow(
          box(
            title = "Intra-Day Performance Chart",
            plotOutput("Plot")),
          box(
            title = "Sector L/S Contribution Table",
            gt::gt_output("TableLS"))
      )
    )


))

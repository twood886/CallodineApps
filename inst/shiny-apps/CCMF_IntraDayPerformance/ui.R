#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

list.pack <- c(
  "shiny",
  "tidyverse",
  "readxl",
  "zoo",
  "scales",
  "magrittr",
  "httr",
  "jsonlite",
  "lubridate",
  "gt",
  "shinydashboard")

new.packages <- list.pack[!(list.pack %in% installed.packages()[,"Package"])]

#install missing ones
if(length(new.packages)) install.packages(new.packages, dependencies = TRUE)

lapply(
  list.pack,
  function(x)
    if(!require(x,character.only = TRUE)) install.packages(x))

library(shiny)
library(shinydashboard)

# Define UI for application that draws a histogram
shinyUI(

  dashboardPage(
    dashboardHeader(title = "Callodine Capital Master Fund Intra-Day Performance"),
    dashboardSidebar(disable = TRUE),
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

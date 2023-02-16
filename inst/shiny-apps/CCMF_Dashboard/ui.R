list.pack <- c(
  "shiny",
  "plotly",
  "tidyverse",
  "readxl",
  "zoo",
  "scales",
  "magrittr",
  "httr",
  "jsonlite",
  "lubridate",
  "gt",
  "shinydashboard",
  "gtExtras",
  "tidyquant")

new.packages <- list.pack[!(list.pack %in% installed.packages()[,"Package"])]

if(length(new.packages)) install.packages(new.packages, dependencies = TRUE)

lapply(
  list.pack,
  function(x)
    if(!require(x,character.only = TRUE)) install.packages(x))

library(tidyverse)
library(lubridate)
library(gt)
library(shiny)
library(gtExtras)
library(plotly)
library(shinydashboard)
library(lubridate)
library(gt)
library(tidyquant)


# Define UI for application that draws a histogram
shinyUI(

  dashboardPage(
    skin = "black",
    dashboardHeader(
      title = "Callodine Capital Master Fund Performance",
      titleWidth = 450),

    dashboardSidebar(
      dateRangeInput(
        'dateRange',
        label = "Select Date Range for Analysis:",
        start = floor_date(Sys.Date()-1, 'year'),
        end = Sys.Date()-1,
        separator = " to ",
        format = "mm/dd/yyyy",
        startview = 'year',
        weekstart = 0),


      # Date Buttons
      actionButton("buttonMTD", "Month to Date", style='padding:4px; font-size:80%'),
      actionButton("button1M", "1 Month", style='padding:4px; font-size:80%'),
      actionButton("buttonQTD", "Quarter to Date", style='padding:4px; font-size:80%'),
      actionButton("button1Y", "1 Year", style='padding:4px; font-size:80%'),
      actionButton("buttonITD", "Inception to Date", style='padding:4px; font-size:80%'),

      sidebarMenu(
        menuItem(
          "Intraday Return",
          tabName = "intraday",
          icon = icon("signal"),
          badgeLabel = "new",
          badgeColor = "green"),
        menuItem(
          "Performance Summary",
          tabName = "psummary",
          icon = icon("signal"),
          badgeLabel = "new",
          badgeColor = "green"),
        menuItem(
          "Portfolio Exposure",
          tabName = "exp",
          icon = icon("th"),
          badgeLabel = "new",
          badgeColor = "green"
        ))),

    dashboardBody(

      tabItems(

        tabItem(
          tabName = "intraday",
          h2("Intraday Return Summary"),

          fluidRow(
            box(
              title = "Intra-Day Return Chart",
              plotlyOutput("idayPlot")
              ),
            box(
              title = "Intra-day Sector L/S Contribution",
              gt_output("idayTableLS")
              ))),

        tabItem(
          tabName = "psummary",
          h2("Performance Summary"),

          fluidRow(
            box(
              title = "Summary Metrics",
              downloadButton('downloadtableSummary',"Download"),
              gt_output("tableSummary"),
              width = 4),
            tabBox(
              title = "Performance Chart",
              id = "tabset1",
              width = 8,
              tabPanel("Cumulative Return", plotlyOutput("chartCumRet")),
              tabPanel("Weekly Return", plotlyOutput("chartWeekReturn")),
              tabPanel("Monthly Return", plotlyOutput("chartMonthReturn"))

              #plotlyOutput("chartCumRet"),
              )),
          fluidRow(
            box(
              title = "Sector Metrics",
              gt_output("sectableSummary")))),

        tabItem(
          tabName = "exp",
          h2("Portfolio Exposure"),
          fluidRow(
            box(
              title = "Gross Portfolio Exposure",
              plotlyOutput("chartGrossExp")),
            box(
              title = "Net Portfolio Exposure",
              plotlyOutput("chartNetExp")))
        )
      )
    )
))

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

      sidebarMenu(
        menuItem(
          "Intraday Return",
          tabName = "intraday",
          icon = icon("signal")),
        menuItem(
          "Performance Summary",
          tabName = "psummary",
          icon = icon("signal")),
        menuItem(
          "Portfolio Exposure",
          tabName = "exp",
          icon = icon("th"),
          badgeLabel = "new",
          badgeColor = "green"),
        menuItem(
          "Sector Exposure",
          tabName = "sectorexp",
          icon = icon("th")))),

    dashboardBody(

      tabItems(

        tabItem(
          tabName = "intraday",
          h2("Intra-Day Return Summary"),

          fluidRow(
            box(
              title = "Intra-Day Return Chart",
              plotlyOutput("idayPlot")),
            box(
              title = "Intra-Day Sector L/S Contribution",
              gt_output("idayTableLS"))),
          fluidRow(
            box(
              title = "Intra-Day Security Contribution",
              gt_output("idayTableSecContr")),
            box(
              title = "Intra-Day Trades",
              gt_output("idayTableTrades")),
          )),

        tabItem(
          tabName = "psummary",
          h2("Performance Summary"),
          fluidRow(
            box(
              width = 12,
              height = 50,
              column(
                width = 3,
                dateRangeInput(
                  'dateRange.psum',
                  label = NULL,
                  start = floor_date(Sys.Date()-1, 'year'),
                  end = Sys.Date()-1,
                  separator = " to ",
                  format = "mm/dd/yyyy",
                  startview = 'year',
                  weekstart = 0)),
              actionButton("buttonMTD.psum", "Month to Date"),
              actionButton("button1M.psum", "1 Month"),
              actionButton("buttonQTD.psum", "Quarter to Date"),
              actionButton("buttonYTD.psum", "Year to Date"),
              actionButton("button1Y.psum", "1 Year"),
              actionButton("buttonITD.psum", "Inception to Date"))),
          fluidRow(
            box(
              title = "Summary Metrics",
              gt_output("tableSummary"),
              downloadButton('downloadtableSummary',"Download"),
              width = 4),
            tabBox(
              title = "Performance Chart",
              id = "tabset1",
              width = 8,
              tabPanel("Cumulative Return", plotlyOutput("chartCumRet")),
              tabPanel("Weekly Return", plotlyOutput("chartWeekReturn")),
              tabPanel("Monthly Return", plotlyOutput("chartMonthReturn")),
              tabPanel("Annual Return", plotlyOutput("chartYearReturn")))),
          fluidRow(
            box(
              width = 8,
              title = "Sector Metrics",
              gt_output("sectableSummary")))),

        tabItem(
          tabName = "exp",
          h2("Portfolio Exposure"),
          fluidRow(
            box(
              width = 12,
              height = 50,
              column(
                width = 3,
                dateRangeInput(
                  'dateRange.pexp',
                  label = NULL,
                  start = floor_date(Sys.Date()-1, 'year'),
                  end = Sys.Date()-1,
                  separator = " to ",
                  format = "mm/dd/yyyy",
                  startview = 'year',
                  weekstart = 0)),
              actionButton("buttonMTD.pexp", "Month to Date"),
              actionButton("button1M.pexp", "1 Month"),
              actionButton("buttonQTD.pexp", "Quarter to Date"),
              actionButton("buttonYTD.pexp", "Year to Date"),
              actionButton("button1Y.pexp", "1 Year"),
              actionButton("buttonITD.pexp", "Inception to Date"))),
          fluidRow(
            box(
              title = "Gross Portfolio Exposure",
              plotlyOutput("chartGrossExp")),
            box(
              title = "Net Portfolio Exposure",
              plotlyOutput("chartNetExp"))),
          fluidRow(
            box(
              title = "Performance vs Net Adj S&P 500",
              plotlyOutput("netAdjPerf")))),

        tabItem(
          tabName = "sectorexp",
          h2("Sector Exposure"),
          fluidRow(
            box(
              width = 3,
              sliderInput(
                "SectorExpSlider",
                label = "Select Date",
                min = as.Date("2018-07-02"),
                max = Sys.Date() -1,
                value = Sys.Date() -1,
                timeFormat = "%b-%d-%y")),
            box(
              width = 9,
              title = "Sector Exposure - Snapshot",
              plotlyOutput("chartSectorWeightSnapshot"))))


      )
    )
))


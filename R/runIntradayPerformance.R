#' @title CCMF Intraday Performance Shiny App
#' @description Runs application to display intraday performance of CCMF
#' @import gt
#' @import shiny
#' @import readxl
#' @import zoo
#' @import scales
#' @import magrittr
#' @import httr
#' @import jsonlite
#' @import lubridate
#' @import tidyverse
#' @import shinydashboard
#' @export
runIntradayPerformance <- function() {
  appDir <- system.file("shiny-apps", "CCMF_IntraDayPerformance", package = "CallodineApps")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `CallodineApps`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal", launch.browser = T)
}

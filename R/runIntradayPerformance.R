#' @title CCMF Intraday Performance Shiny App
#' @description Runs application to display intraday performance of CCMF
#' @import tidyverse
#' @import shiny
#' @import yahoofinancer
#' @import readxl
#' @import zoo
#' @import scales
#' @export
runIntradayPerformance <- function() {
  appDir <- system.file("shiny-apps", "CCMF_IntraDayPerformance", package = "Callodine_Apps")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `Callodine_Apps`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}

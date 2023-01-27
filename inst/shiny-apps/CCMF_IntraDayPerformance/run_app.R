#' Run the Shiny Application
#'
#' @param options optional, described in ?shiny::shinyApp
#'
#' @export
run_app <- function(options = list()) {
  shiny::shinyApp(ui = shinyUI,
                  server = shinyServer,
                  options = options)

}

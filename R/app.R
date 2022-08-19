#' Launch Shiny App
#'
#' @param name The name of the app to run
#' @param ... arguments to pass to shiny::runApp
#'
#' @export
#'
app <- function(name = "nowcast_gmi", ...) {
  appDir <- system.file(paste0("apps/", name), package = "nowcastapp")
  if (appDir == "") stop("The shiny app ", name, " does not exist")
  shiny::runApp(appDir, ...)
}

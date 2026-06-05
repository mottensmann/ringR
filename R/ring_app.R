#' Launch shiny app
#' @export
ring_app <- function() {
    suppressMessages(shiny::runApp(system.file("shiny/dbView", package = "ringR")))
}

#' Run webGQT Shiny app
#'
#' @param gqt_path Local path for the gqt installation
#'
#' @return Opens a shiny app in the web browser
#' @export
#'
#' @examples
#' run_webgqt(gqt_path = '/usr/local/gqt/bin/gqt')

run_webgqt <- function(gqt_path = "/usr/local/gqt/bin/gqt") {
  shiny::shinyOptions(gqt_path = gqt_path)
  source(system.file('extdata', "webgqt/app.R", package = "webGQT"))$value

}
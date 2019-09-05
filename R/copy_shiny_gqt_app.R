#' Copy shiny app to user's local server
#'
#' @param copy_to  path to shiny-server app location on user's local server
#'
#' @return     Installs webGQT on user's server
#' @export
#'
#' @examples
#' copy_shiny_gqt_app(copy_to = '/srv/shiny-server/')
#' 

copy_shiny_gqt_app <- function(copy_to){
  file.copy(from = system.file('extdata', 'webgqt', package = 'webGQT'), to = copy_to,recursive = TRUE)
}
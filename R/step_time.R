#' step_time
#'
#' Function to print a step and time
#'
#' @param step String, representing the step. Can be a number too
#'
#' @return String, with step number and the date + time
#' @export
#'
#' @examples
#' step_time(1.1)
step_time <- function(step = 0){
  paste0("(",step,") - [ ",  base::format(Sys.time(), "%d/%m/%Y %H:%M:%S ]"))
}

#' step_time
#'
#' Function to print a step and time. Step is by default with 2 digits
#'
#' @param step double, representing the step.
#' @param digits int, number of digits we will write down
#'
#' @return String, with step number and the date + time
#' @export
#'
#' @examples
#' step_time(1.1)
step_time <- function(step = 0, digits = 2){
  paste0("(",
         sprintf(paste0("%.",digits,"f"),
                 round(step,digits)),
         ") - [ ",
         base::format(Sys.time(),
                      "%d/%m/%Y %H:%M:%S ]"))
}


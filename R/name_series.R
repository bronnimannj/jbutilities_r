#' name_series
#'
#' Function to create files without over writing them.
#'
#' It will add an underscore and the next number of the series.
#'
#' @param name_file String, representing the name of the file
#' @param type_file String, representing the extension of the file
#'
#' @return String, with the next name of the series
#' @export
#'
#' @examples
#' name_series("test", "csv")
name_series <- function(name_file, type_file) {
  if(length(name_file) == 0 | length(type_file) == 0){
    stop("We need name_file and type_file to be of length 1 at least")
  }

  f <- paste0(name_file, ".", type_file)
  if (!file.exists(f)) {
    return(f)
  }
  i <- 1
  repeat {
    f <- paste0(name_file, "_", i, ".", type_file)
    if (!file.exists(f)) {
      return(f)
    }
    i <- i + 1
  }
}

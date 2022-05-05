#' find_series
#'
#' Function to find the latest file in a series. Types recognized are:
#'
#' @param name_file String, representing the name of the file. If file not at the working directory, add the path to it.
#' @param type_file String, representing the extension of the file
#'
#' @return String, with the last name of the series
#' @export
#'
#' @examples
#' find_series("test", "csv")
find_series <- function(name_file, type_file) {
  if(length(name_file) == 0 | length(type_file)== 0 ){
    stop("We need to have something to write")
  }

  f <- paste0(name_file, ".", type_file)
  if (!file.exists(f)) {
    return(NULL)
  }
  i <- 1
  repeat {
    f1 <- paste0(name_file, "_", i, ".", type_file)
    if (!file.exists(f1)) {
      return(f)
    }
    i <- i + 1
    f <- f1
  }
}

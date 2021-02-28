#' name_series
#'
#' Function to create files without over writing them.
#'
#' It will add an underscore and the next number of the series.
#'
#' @param Name String, representing the name of the file
#' @param Type String, representing the extension of the file
#'
#' @return String, with the next name of the series
#' @export
#'
#' @examples
#' name_series("test", "csv")
name_series <- function(Name, Type) {
  if(length(Name) == 0 | length(Type) == 0){
    stop("We need Name and Type to be of length 1 at least")
  }

  f <- paste0(Name, ".", Type)
  if (!file.exists(f)) {
    return(f)
  }
  i <- 1
  repeat {
    f <- paste0(Name, "_", i, ".", Type)
    if (!file.exists(f)) {
      return(f)
    }
    i <- i + 1
  }
}

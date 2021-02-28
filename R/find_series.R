#' find_series
#'
#' Function to find the latest file in a series. Types regognized are:
#'
#' @param Name String, representing the name of the file
#' @param Type String, representing the extension of the file
#'
#' @return String, with the next name of the series
#' @export
#'
#' @examples
#' find_series("test", "csv")
find_series <- function(Name, Type) {
  if(length(Name) == 0 | length(Type)== 0 ){
    stop("We need to have something to write")
  }

  f <- paste0(Name, ".", Type)
  if (!file.exists(f)) {
    return(NULL)
  }
  i <- 1
  repeat {
    f1 <- paste0(Name, "_", i, ".", Type)
    if (!file.exists(f1)) {
      return(f)
    }
    i <- i + 1
    f <- f1
  }
}

#' Fast cut function
#'
#' This function is a fast version of cut that will put every element of x in one of the intervals created by breaks.
#'
#' Each interval is left closed and right opened, example: '[0,1)'.
#'
#' If -Inf or/and +Inf are not in the vector 'breaks', it will be added to it.
#'
#' If none of the elements are out of the intervales between the breaks,
#' then the factor returned is only constituted of the breaks intervals. Else, +/-Inf will be added.
#'
#' @param x Numeric vector of elements to put into segments
#' @param breaks Numeric vector of breaks
#'
#' @return factor of intervals
#' @export
#'
#' @examples cut_fast(c(0,0.2,3,5),c(0,1,2,3,4)) #will contain +Inf
#' cut_fast(c(0,0.2,3),c(0,1,2,3,4)) #will only contain the specific intervals
#' cut_fast(c(-1,0.2,3),c(0,1,2,3,4)) #will contain -Inf
#'
cut_fast <- function(x = c(), breaks = c()) {
  if(length(breaks)<= 1){
    stop("We need at least 2 values for breaks")
  }
  if(length(x)<= 0){
    stop("We need at least 1 value for x")
  }
  if(!is.numeric(x)){
    stop("We need x to be numeric")
  }
  if(!is.numeric(breaks)){
    stop("We need breaks to be numeric")
  }


  if(!-Inf %in% breaks){
    breaks <- unique(c(-Inf,breaks))
  }
  if(!Inf %in% breaks){
    breaks <- unique(c(breaks, Inf))
  }
  labels <- paste0("[", breaks[-length(breaks)], ",", breaks[-1L], ")")

  vector_output <- labels[findInterval(x, breaks)]

  return(factor(vector_output,
                levels = unique(vector_output)))
}

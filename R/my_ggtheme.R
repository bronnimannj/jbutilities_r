#' my_ggtheme
#'
#' Function to put my favorite ggplot theme to any ggplot plot
#'
#' @param x_percent boolean, if TRUE we label the x axis into percents
#' @param y_percent boolean, if TRUE we label the y axis with percents
#'
#' @return a ggplot with a new theme
#' @export
#'
#' @examples
#' ggplot2::ggplot(data.frame(a = c(1,2),b = c(3,4)), ggplot2::aes(a,b)) + my_ggtheme()
my_ggtheme <- function(x_percent = FALSE,
                       y_percent = FALSE){

  p1 <- ggplot2::theme_light() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5,
                                    face = "bold"))

  if(x_percent){
    p1 <- p1 +
      ggplot2::scale_x_continuous(labels = scales::percent)
  }

  if(y_percent){
    p1 <- p1 +
      ggplot2::scale_y_continuous(labels = scales::percent)
  }

  p1
}

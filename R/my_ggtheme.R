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
#' ggplot(data.frame(a = c(1,2),b = c(3,4)), aes(a,b)) + my_ggtheme()
my_ggtheme <- function(x_percent = FALSE,
                       y_percent = FALSE){

  p1 <- theme_light() +
    theme(plot.title = element_text(hjust = 0.5,
                                    face = "bold"))

  if(x_percent){
    p1 <- p1 +
      scale_x_continuous(labels = scales::percent)
  }

  if(y_percent){
    p1 <- p1 +
      scale_y_continuous(labels = scales::percent)
  }

  p1
}

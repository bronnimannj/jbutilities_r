#' glm_correct_pred
#'
#' Correct the predictions of a glm after an over-sampling step.
#'
#' Formula is based on:
#' www.data-mining-blog.com/tips-and-tutorials/overrepresentation-oversampling/
#'
#' @param predictions numerical vector, glm predictions
#' @param original_avg double, the original average prediction
#' @param new_avg double, the new average prediction
#'
#' @return numerical vector, new corrected glm predictions
#' @export
#'
#' @example glm_correct_pred(c(0.1,0.2,0.3), 0.01, 0.5)
glm_correct_pred <- function(preds, original_avg, new_avg) {
  return(1 / (1 + (1 / original_avg - 1) / (1 / new_avg - 1) * (1 / preds - 1)))
}

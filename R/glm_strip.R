#' glm_strip
#'
#' GLM models keep track of the training data and many other details.
#' This function cuts down all "un-necessary" details from the GLM.
#'
#' @param glm_model GLM model from stats package.
#'
#' @return glm model
#' @export
#'
glm_strip <- function(glm_model) {
  glm_model$y = c()
  glm_model$model = c()

  glm_model$residuals = c()
  glm_model$fitted.values = c()
  glm_model$effects = c()
  glm_model$qr$qr = c()
  glm_model$linear.predictors = c()
  glm_model$weights = c()
  glm_model$prior.weights = c()
  glm_model$data = c()

  glm_model$family$variance = c()
  glm_model$family$dev.resids = c()
  glm_model$family$aic = c()
  glm_model$family$validmu = c()
  glm_model$family$simulate = c()
  attr(glm_model$terms,".Environment") = c()
  attr(glm_model$formula,".Environment") = c()

  glm_model
}

#' Calculate root-mean-square error metric
#'
#' @param true the true response values
#' @param pred the predicted response values
#' @export

rmse <- function (true, pred) {
  return(sqrt(mean((true - pred)^2)));
}

#' Calculate log-loss error metric
#'
#' @param true the true response values
#' @param pred the predicted response values
#' @param eps a lower bound on predicted probabilities
#' @export

logloss <- function (true, pred, eps=1e-15) {
  pred <- pmin(pmax(pred, eps), 1-eps);
  return(-mean(true * log(pred)) - mean((1 - true) * log(1 - pred)));
}

#' Calculate mean absolute error metric
#'
#' @param true the true response values
#' @param pred the predicted response values
#' @export

mae <- function (true, pred) {
  return(mean(abs(true - pred)));
}
# Wrapper functions for uncooperative base models

#' Trivial regression model which returns a given constant
#'
#' @param x the matrix of predictors
#' @param y the vector of responses
#' @param k the constant to return
#' @export
constant <- function (x, y, k) {
  model <- list(k=k);
  class(model) <- 'constant';
  return(model);
}

#' Trivial regression model which returns a given constant
#'
#' @param object the model object
#' @param newdata the matrix of new predictors
#' @param ... other parameters (unused)
#' @method predict constant
#' @export
predict.constant <- function(object, newdata, ...) {
  return(rep(object$k, nrow(newdata)));
}

#' Wrapper for loess fit
#'
#' @param x the matrix of predictors
#' @param y the vector of responses
#' @param ... other parameters to pass to \code{loess()}
#' @export
loess.fit <- function(x, y, ...) {
  model <- loess(y ~ x, ...);
}    
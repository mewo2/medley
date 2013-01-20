\name{add.medley}
\alias{add.medley}
\title{Add a new model to an existing medley}
\usage{
  add.medley(object, model, args = list(),
    predict.args = list(), feature.subset = NULL,
    folds = object$folds, postprocess = c())
}
\arguments{
  \item{object}{the medley to be added to}

  \item{model}{the model fitting function}

  \item{args}{a list of extra arguments to \code{model}}

  \item{predict.args}{a list of extra arguments to the
  predict function for \code{model}}

  \item{feature.subset}{a subset of the features to use}

  \item{folds}{the number of cross-validation folds to use}

  \item{postprocess}{an optional function to apply to the
  predicted values}
}
\description{
  Add a new model to an existing medley
}


\name{predict.medley}
\alias{predict.medley}
\title{Make a prediction based on a medley}
\usage{
  \method{predict}{medley} (object, newdata, seed = c(),
    min.members = 5, max.members = 100, baggs = 1,
    mixer = mean, ...)
}
\arguments{
  \item{object}{the medley to predict from}

  \item{newdata}{a matrix of new predictor data}

  \item{seed}{indices of models which should be used for an
  initial ensemble}

  \item{min.members}{the minimum number of models in an
  ensemble (selected randomly)}

  \item{max.members}{the maximum number of models in an
  ensemble}

  \item{baggs}{the number of bagging iterations to perform}

  \item{mixer}{a function to combine model predictions}

  \item{...}{other arguments (unused)}
}
\description{
  Make a prediction based on a medley
}


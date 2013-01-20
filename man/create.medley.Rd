\name{create.medley}
\alias{create.medley}
\title{Create a new (empty) medley object}
\usage{
  create.medley(x, y, label = "", errfunc = rmse,
    base.model = NULL, folds = 8)
}
\arguments{
  \item{x}{matrix of predictors}

  \item{y}{vector of response values}

  \item{label}{a unique label for this medley (used in
  status messages)}

  \item{errfunc}{an error metric for this medley}

  \item{base.model}{a function to use as a baseline model}

  \item{folds}{the default number of cross-validation folds
  to use}
}
\description{
  Create a new (empty) medley object
}
\examples{
require(e1071);
data(swiss);
x <- swiss[,1:5];
y <- swiss[,6];
train <- sample(nrow(swiss), 30);
m <- create.medley(x[train,], y[train]);
for (gamma in c(1e-3, 2e-3, 5e-3, 1e-2, 2e-2, 5e-2, 1e-1, 2e-1, 5e-1, 1)) {
  m <- add.medley(m, svm, list(gamma=gamma));
}
p <- predict(m, x[-train,]);
rmse(p, y[-train]);
}


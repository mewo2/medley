## What is Medley? ##

Medley is an R package which implements the Caruana et al., 2004 algorithm for
greedy stepwise ensemble selection for regression models.

The idea behind medley is to make the creation of "ensembles" or "blends" of
models as simple as possible. Individual models can be produced by varying
hyperparameters or input feature sets, as well as by changing the underlying
model code.

## Usage example ##

    require(medley);
    require(randomForest);
    require(e1071);
    
    # x and y are the training predictors and responses respectively
    m <- create.medley(x, y, errfunc=rmse);

    # add SVMs for a variety of gamma parameters
    for (g in 1:10) {
      m <- add.medley(m, svm, list(gamma=1e-3 * g));
    }

    # add random forests with varying mtry parameter
    for (mt in c(5, 10, 20, 50)) {
      m <- add.medley(m, randomForest, list(mtry=mt));
    }

    # use only the best 80% of individual models
    m <- prune.medley(m, 0.8);

    # predict using new predictor matrix newx
    p <- predict(m, newx);


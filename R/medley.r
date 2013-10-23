#' Create a new (empty) medley object
#'
#' @param x matrix of predictors
#' @param y vector of response values
#' @param label a unique label for this medley (used in status messages)
#' @param errfunc an error metric for this medley
#' @param base.model a function to use as a baseline model
#' @param folds the default number of cross-validation folds to use
#' @export
#' @examples
#' require(e1071);
#' data(swiss);
#' x <- swiss[,1:5];
#' y <- swiss[,6];
#' train <- sample(nrow(swiss), 30);
#' m <- create.medley(x[train,], y[train]);
#' for (gamma in c(1e-3, 2e-3, 5e-3, 1e-2, 2e-2, 5e-2, 1e-1, 2e-1, 5e-1, 1)) {
#'   m <- add.medley(m, svm, list(gamma=gamma));
#' }
#' p <- predict(m, x[-train,]);
#' rmse(p, y[-train]);
create.medley <- function (x, y, label='', errfunc=rmse, base.model=NULL, folds=8) {
  if (!is.null(base.model)) {
    base.y <- predict(base.model, x);
  } else {
    base.y <- rep(0, length(y));
  }
  object <- list(
    x=as.matrix(x), 
    y=y,
    base.y=base.y,
    mod.y=y - base.y, 
    errfunc=errfunc, 
    models=list(), 
    args=list(), 
    predict.args=list(),
    feature.subset=list(), 
    fitted=list(), 
    cv=list(),
    base.model=base.model,
    label=label,
    folds=folds,
    postprocess=list()
  );
  class(object) <- 'medley';
  return(object);
}

#' Recursively combine two or more medley objects for the same problem
#'
#' @param e1 the first medley to combine
#' @param e2 the second medley to combine
#' @param ... further medleys to combine
#' @export
combine.medley <- function (e1, e2=NULL, ...) {
  if (is.null(e2)) return(e1);
  
  e1$models <- c(e1$models, e2$models);
  e1$args <- c(e1$args, e2$args);
  e1$predict.args <- c(e1$predict.args, e2$predict.args);
  e1$feature.subset <- c(e1$feature.subset, e2$feature.subset);
  e1$fitted <- c(e1$fitted, e2$fitted);
  e1$cv <- c(e1$cv, e2$cv);
  e1$postprocess <- c(e1$postprocess, e2$postprocess);
  return(combine.medley(e1, ...));
}

#' Add a new model to an existing medley
#'
#' @param object the medley to be added to
#' @param model the model fitting function
#' @param args a list of extra arguments to \code{model}
#' @param predict.args a list of extra arguments to the predict function for \code{model}
#' @param feature.subset a subset of the features to use
#' @param folds the number of cross-validation folds to use
#' @param postprocess an optional function to apply to the predicted values
#' @export
add.medley <- function (object, model, args=list(), predict.args=list(), feature.subset=NULL, folds=object$folds, postprocess=c()) {
  if (is.null(feature.subset)) {
    feature.subset <- c(1:ncol(object$x));
  }
  n <- length(object$models) + 1;
  object$models[[n]] <- model;
  object$args[[n]] <- args;
  object$predict.args[[n]] <- predict.args;
  object$feature.subset[[n]] <- feature.subset;
  object$postprocess[[n]] <- function(x) x;

  pt <- proc.time()[[3]];
  # Fit model to all data
  data <- list(x=object$x[,feature.subset], y=object$mod.y);
  object$fitted[[n]] <- do.call(model, c(data, args));
  
  # Do cross-validation
  k <- length(object$y);
  cv.sets <- (seq_len(k) %% folds) + 1;
  
  pred <- numeric(k);
  for (i in 1:folds) {
    holdout <- which(cv.sets == i);
    data <- list(x=object$x[-holdout,feature.subset], y=object$mod.y[-holdout]);
    fitted <- do.call(model, c(data, args));
    preddata <- list(fitted, newdata=object$x[holdout,feature.subset])
    pred[holdout] <- do.call(predict, c(preddata, predict.args));
  }
  object$cv[[n]] <- pred + object$base.y;
  pt <- proc.time()[[3]] - pt;
  cat(object$label, 'CV model', n, class(object$fitted[[n]]), substring(deparse(args, control=NULL), 5), 'time:', pt, 'error:', object$errfunc(object$y, object$cv[[n]]), '\n');
    
  nn <- n;
  for (pp in postprocess) {
    nn <- nn + 1;
    object$models[[nn]] <- model;
    object$args[[nn]] <- args;
    object$predict.args[[nn]] <- predict.args;
    object$feature.subset[[nn]] <- feature.subset;
    object$postprocess[[nn]] <- pp;
    object$fitted[[nn]] <- object$fitted[[n]];
    object$cv[[nn]] <- pp(pred) + object$base.y;
    cat(object$label, 'PP error:', object$errfunc(object$y, object$cv[[nn]]), '\n');
  }
  
  return(object);
}

#' Prune the models in a medley
#'
#' @param object the medley to prune
#' @param prune.factor the proportion of the models to keep
#' @export
prune.medley <- function (object, prune.factor=0.2) {
  n <- ceiling(length(object$models) * prune.factor);
  errs <- sapply(object$cv, function(pred) object$errfunc(object$y, pred));
  keep <- order(errs)[1:n];
  object$models <- object$models[keep];
  object$args <- object$args[keep];
  object$predict.args <- object$predict.args[keep];
  object$feature.subset <- object$feature.subset[keep];
  object$fitted <- object$fitted[keep];
  object$cv <- object$cv[keep];
  return(object);
}

#' Make a prediction based on a medley
#'
#' @param object the medley to predict from
#' @param newdata a matrix of new predictor data
#' @param seed indices of models which should be used for an initial ensemble
#' @param min.members the minimum number of models in an ensemble (selected randomly)
#' @param max.members the maximum number of models in an ensemble
#' @param baggs the number of bagging iterations to perform
#' @param mixer a function to combine model predictions
#' @param ... other arguments (unused)
#' @method predict medley
#' @export
predict.medley <- function (object, newdata, seed=c(), min.members=5, max.members=100, baggs=1, mixer=mean, ...) {
  newdata <- as.matrix(newdata);
  best.errs <- c();
  big.mix <- c();
  for (i in 1:baggs) {
    if (baggs == 1) {
      s <- 1:length(object$y);
    } else {
      s <- sample(length(object$y), replace=T);
    }
    model.sample <- sample(length(object$models), length(object$models) * .8);
    cat('Sampled...');
    if (length(seed) == 0) {
      errs <- sapply(object$cv[model.sample], function(pred) object$errfunc(object$y[s], pred[s]));
      min.members <- min(length(errs), min.members)
      mix <- model.sample[order(errs)[1:min.members]];
    
    } else {
      mix <- seed;
    }
    mixpred <- simplify2array(object$cv[mix]);
    
    best.err <- object$errfunc(object$y[s], apply(mixpred, 1, mixer)[s]);
    
    while(length(mix) < max.members) {
      errs <- sapply(object$cv[model.sample], function(pred) object$errfunc(object$y[s], apply(cbind(mixpred, pred), 1, mixer)[s]));
      best <- which.min(errs);
      if (best.err <= errs[best]) break;
      mix <- c(mix, model.sample[best]);
      best.err <- errs[best];
      mixpred <- simplify2array(object$cv[mix]);
    }
    predictions <- list();
    for (i in unique(sort(mix))) {
      preddata <- list(object$fitted[[i]], newdata=newdata[,object$feature.subset[[i]]]);
      predictions[[i]] <- object$postprocess[[i]](do.call(predict, c(preddata, object$predict.args[[i]])));
      frac <- mean(mix == i);
      cat(format(frac*100, digits=1, nsmall=2), '%: ', i, class(object$fitted[[i]]), substring(deparse(object$args[[i]], control=NULL), 5), '\n');
    }
    best.err <- object$errfunc(object$y, apply(mixpred, 1, mixer));
    cat('CV error:', best.err, '\n');
    if (length(unique(mix)) == 1) {pred <- predictions[[mix]]}
    else {pred <- apply(simplify2array(predictions[mix]), 1, mixer)}
    best.errs <- c(best.errs, best.err);
    big.mix <- cbind(big.mix, pred);
  }
  pred <- apply(big.mix, 1, mixer);
  if (!is.null(object$base.model)) {
    pred <- pred + predict(object$base.model, newdata);
  }
  attr(pred, 'cv.err') <- mean(best.errs);
  return(pred);
}
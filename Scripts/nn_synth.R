## ---------------------------------------------
## 
## Script: nn_synth.R
## 
## Author: Sophie Furlow
## Date created: October 22, 2025
##
## Description: 
##
## ---------------------------------------------

library(nnet)

# helper: ensure factors in xp have same levels as in x
.align_levels <- function(x, xp) {
  for (nm in intersect(names(x), names(xp))) {
    if (is.factor(x[[nm]])) {
      xp[[nm]] <- factor(xp[[nm]], levels = levels(x[[nm]]))
    }
  }
  xp
}


# syn.nn = custom synthesizer of a one-layer neural network
syn.nn <- function(y, x, xp,
                   proper = FALSE,
                   size = 5,        # hidden units
                   decay = 1e-4,    # L2 regularization
                   maxit = 500,
                   trace = FALSE,
                   MaxNWts = 30000, # increase if you have many dummies
                   ...) {

  # coerce to data.frame and align factor levels between x and xp
  x  <- as.data.frame(x)
  xp <- as.data.frame(xp)
  xp <- .align_levels(x, xp)

  # bootstrap rows for "proper" synthesis (adds extra variability)
  # (cf. synthpop's proper=TRUE in built-in methods)
  if (proper) {
    s <- sample(seq_len(nrow(x)), replace = TRUE)
    x_boot <- x[s, , drop = FALSE]
    y_boot <- y[s]
  } else {
    x_boot <- x
    y_boot <- y
  }

  terms_obj <- terms(~ ., data = x_boot)
  X  <- model.matrix(terms_obj, data = x_boot)
  Xp <- model.matrix(terms_obj, data = xp)

  center <- colMeans(X)
  scalev <- apply(X, 2, sd)
  scalev[scalev == 0 | is.na(scalev)] <- 1
  Xs  <- scale(X,  center = center, scale = scalev)
  Xps <- scale(Xp, center = center, scale = scalev)

  # regression case ----------------------------------------------------------
  if (is.numeric(y)) {
    fit <- nnet(x = Xs, y = y_boot,
                size = size, decay = decay, maxit = maxit,
                linout = TRUE, trace = trace, MaxNWts = MaxNWts)

    pred <- as.numeric(predict(fit, Xps, type = 'raw'))

    # residual noise so synthetic values aren't deterministic
    resid_sd <- stats::sd(y_boot - as.numeric(predict(fit, Xs, type = 'raw')))
    if (!is.finite(resid_sd) || resid_sd == 0) resid_sd <- 1e-6
    res <- pred + stats::rnorm(length(pred), sd = resid_sd)

    return(list(res = res, fit = list(model = fit,
                                      terms = terms_obj,
                                      center = center, scale = scalev)))
  }

  # classification case ------------------------------------------------------
  yf <- factor(y_boot) # just in case not already factored
  K  <- length(levels(yf))

  # class indicator matrix for softmax
  Y_ind <- class.ind(yf)

  fit <- nnet(x = Xs, y = Y_ind, size = size, decay = decay,
              maxit = maxit, softmax = TRUE, trace = trace, MaxNWts = MaxNWts)

  prob <- predict(fit, Xps, type = 'raw')   # matrix n x K
  if (is.vector(prob)) prob <- matrix(prob, ncol = K)

  # row-wise categorical draws
  u <- runif(nrow(prob))
  cs <- t(apply(prob, 1, cumsum))
  draw_idx <- 1 + rowSums(cs < u)
  res <- factor(levels(yf)[draw_idx], levels = levels(yf))

  return(list(res = res, fit = list(model = fit,
                                    levels = levels(yf),
                                    terms = terms_obj,
                                    center = center, scale = scalev)))
}

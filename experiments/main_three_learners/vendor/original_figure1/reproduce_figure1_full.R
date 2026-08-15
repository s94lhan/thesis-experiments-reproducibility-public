required_packages <- c(
  "data.table",
  "ggplot2",
  "mgcv",
  "randomForest",
  "earth",
  "glmnet",
  "gbm",
  "xgboost",
  "nnls"
)
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  stop("Install missing packages first: ", paste(missing_packages, collapse = ", "))
}

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(mgcv)
  library(randomForest)
  library(earth)
  library(glmnet)
  library(gbm)
  library(xgboost)
  library(nnls)
})

if (!exists("causalCalibrate")) source("R/causalCalibration.R")
if (!exists("cross_calibrate")) source("R/cross_calibrate.R")

scenario1 <- function(n, seed, draw_outcome = TRUE) {
  set.seed(seed)
  W <- data.frame(
    W1 = runif(n, -1, 1),
    W2 = runif(n, -1, 1),
    W3 = runif(n, -1, 1),
    W4 = runif(n, -1, 1)
  )
  Qbar <- function(a, w) {

    plogis(
      1.5 + 1.5 * a +
        2 * a * abs(w$W1) * abs(w$W2) -
        2.5 * (1 - a) * abs(w$W2) * w$W3 +
        2.5 * w$W3 +
        2.5 * (1 - a) * sqrt(abs(w$W4)) -
        1.5 * a * (w$W2 < 0.5) +
        1.5 * (1 - a) * (w$W4 < 0)
    )
  }
  gbar <- function(w) {
    plogis(-0.25 - w$W1 + 0.5 * w$W2 - w$W3 + 0.5 * w$W4)
  }

  g <- gbar(W)
  Q1 <- Qbar(1, W)
  Q0 <- Qbar(0, W)
  tau <- Q1 - Q0

  if (draw_outcome) {
    A <- rbinom(n, 1, g)
    Y1 <- rbinom(n, 1, Q1)
    Y0 <- rbinom(n, 1, Q0)
    Y <- A * Y1 + (1 - A) * Y0
  } else {
    A <- rep(0, n)
    Y1 <- rbinom(n, 1, Q1)
    Y0 <- rbinom(n, 1, Q0)
    Y <- rep(0, n)
  }

  list(W = W, A = A, Y = Y, Y1 = Y1, Y0 = Y0, Q1 = Q1, Q0 = Q0, g = g, tau = tau)
}

make_folds <- function(n, k, seed) {
  set.seed(seed)
  split(sample(seq_len(n)), rep(seq_len(k), length.out = n))
}

clip_prob <- function(x) {
  pmin(pmax(as.numeric(x), 0.01), 0.99)
}

make_matrix <- function(W) {
  x <- as.matrix(W)
  storage.mode(x) <- "double"
  x
}

constant_model <- function(y, binary) {
  value <- mean(as.numeric(y))
  if (binary) value <- clip_prob(value)
  list(type = "constant", value = value, binary = binary)
}

gam_formula <- function(W) {
  terms <- vapply(names(W), function(name) {
    unique_n <- length(unique(W[[name]]))
    if (unique_n > 4) {
      paste0("s(", name, ", k = ", min(5, unique_n), ")")
    } else {
      name
    }
  }, character(1))
  as.formula(paste("outcome ~", paste(terms, collapse = " + ")))
}

fit_base_model <- function(candidate, W, y, binary) {
  W <- as.data.frame(W)
  y <- as.numeric(y)

  if (!all(is.finite(y)) || length(unique(y)) < 2) {
    return(constant_model(y, binary))
  }

  if (candidate == "glm") {
    df <- data.frame(W, outcome = y)
    fit <- suppressWarnings(if (binary) {
      glm(outcome ~ ., data = df, family = binomial())
    } else {
      glm(outcome ~ ., data = df)
    })
    return(list(type = "glm", fit = fit, binary = binary))
  }

  if (candidate == "glmnet") {
    x <- make_matrix(W)
    family <- if (binary) "binomial" else "gaussian"
    nfolds <- min(5, max(3, floor(nrow(W) / 5)))
    fit <- suppressWarnings(cv.glmnet(x, y, family = family, alpha = 1, nfolds = nfolds))
    return(list(type = "glmnet", fit = fit, binary = binary))
  }

  if (candidate == "gam") {
    df <- data.frame(W, outcome = y)
    fit <- suppressWarnings(if (binary) {
      gam(gam_formula(W), data = df, family = binomial(), method = "REML")
    } else {
      gam(gam_formula(W), data = df, method = "REML")
    })
    return(list(type = "gam", fit = fit, binary = binary))
  }

  if (candidate == "rf") {
    mtry <- max(1, floor(sqrt(ncol(W))))
    if (binary) {
      fit <- suppressWarnings(randomForest(
        x = W,
        y = factor(y, levels = c(0, 1)),
        ntree = 200,
        nodesize = 5,
        mtry = mtry
      ))
    } else {
      fit <- suppressWarnings(randomForest(x = W, y = y, ntree = 200, nodesize = 5, mtry = mtry))
    }
    return(list(type = "rf", fit = fit, binary = binary))
  }

  if (candidate == "mars") {
    df <- data.frame(W, outcome = y)
    fit <- suppressWarnings(if (binary) {
      earth(outcome ~ ., data = df, degree = 2, glm = list(family = binomial))
    } else {
      earth(outcome ~ ., data = df, degree = 2)
    })
    return(list(type = "mars", fit = fit, binary = binary))
  }

  if (grepl("^xgb[0-9]+$", candidate)) {
    depth <- as.integer(sub("^xgb", "", candidate))
    dtrain <- xgb.DMatrix(data = make_matrix(W), label = y)
    params <- list(
      objective = if (binary) "binary:logistic" else "reg:squarederror",
      eval_metric = if (binary) "logloss" else "rmse",
      max_depth = depth,
      eta = 0.03,
      subsample = 0.8,
      colsample_bytree = 1,
      nthread = 1,
      verbosity = 0
    )
    fit <- suppressWarnings(xgb.train(params = params, data = dtrain, nrounds = 250, verbose = 0))
    return(list(type = "xgb", fit = fit, binary = binary))
  }

  stop("Unknown candidate learner: ", candidate)
}

predict_base_model <- function(model, W_new) {
  W_new <- as.data.frame(W_new)

  pred <- suppressWarnings(switch(
    model$type,
    constant = rep(model$value, nrow(W_new)),
    glm = predict(model$fit, newdata = W_new, type = if (model$binary) "response" else "response"),
    glmnet = as.vector(predict(model$fit, newx = make_matrix(W_new), s = "lambda.min", type = "response")),
    gam = predict(model$fit, newdata = W_new, type = if (model$binary) "response" else "response"),
    rf = {
      if (model$binary) {
        as.numeric(predict(model$fit, newdata = W_new, type = "prob")[, "1"])
      } else {
        as.numeric(predict(model$fit, newdata = W_new))
      }
    },
    mars = as.vector(predict(model$fit, newdata = W_new, type = if (model$binary) "response" else "response")),
    xgb = as.vector(predict(model$fit, newdata = xgb.DMatrix(data = make_matrix(W_new)))),
    stop("Unknown fitted model type: ", model$type)
  ))

  pred <- as.numeric(pred)
  finite <- is.finite(pred)
  if (!all(finite)) {
    replacement <- if (any(finite)) mean(pred[finite]) else 0
    pred[!finite] <- replacement
  }
  if (model$binary) pred <- clip_prob(pred)
  pred
}

fit_super_learner <- function(W, y, candidates, binary, seed, cv_folds = 5) {
  W <- as.data.frame(W)
  y <- as.numeric(y)
  n <- nrow(W)

  if (!all(is.finite(y)) || length(unique(y)) < 2) {
    return(list(type = "sl", models = list(constant_model(y, binary)), weights = 1, binary = binary))
  }

  set.seed(seed)
  cv_folds <- min(cv_folds, n)
  fold_id <- sample(rep(seq_len(cv_folds), length.out = n))
  pred_mat <- matrix(NA_real_, nrow = n, ncol = length(candidates))
  colnames(pred_mat) <- candidates

  for (j in seq_along(candidates)) {
    candidate <- candidates[j]
    for (fold in seq_len(cv_folds)) {
      train <- fold_id != fold
      valid <- fold_id == fold
      fit <- tryCatch(
        fit_base_model(candidate, W[train, , drop = FALSE], y[train], binary),
        error = function(e) NULL
      )
      if (is.null(fit)) next
      pred <- tryCatch(
        predict_base_model(fit, W[valid, , drop = FALSE]),
        error = function(e) rep(NA_real_, sum(valid))
      )
      pred_mat[valid, j] <- pred
    }
  }

  keep <- which(apply(pred_mat, 2, function(col) all(is.finite(col)) && sd(col) > 1e-12))
  if (length(keep) == 0) {
    return(list(type = "sl", models = list(constant_model(y, binary)), weights = 1, binary = binary))
  }

  cv_preds <- pred_mat[, keep, drop = FALSE]
  nnls_fit <- tryCatch(nnls(cv_preds, y), error = function(e) NULL)
  weights <- if (is.null(nnls_fit)) rep(0, ncol(cv_preds)) else coef(nnls_fit)

  if (!all(is.finite(weights)) || sum(weights) <= 0) {
    risks <- colMeans((cv_preds - y)^2)
    weights <- rep(0, ncol(cv_preds))
    weights[which.min(risks)] <- 1
  } else {
    weights <- weights / sum(weights)
  }

  full_models <- vector("list", length(keep))
  full_weights <- numeric(length(keep))
  model_count <- 0
  for (pos in seq_along(keep)) {
    fit <- tryCatch(
      fit_base_model(candidates[keep[pos]], W, y, binary),
      error = function(e) NULL
    )
    if (!is.null(fit)) {
      model_count <- model_count + 1
      full_models[[model_count]] <- fit
      full_weights[model_count] <- weights[pos]
    }
  }

  if (model_count == 0 || sum(full_weights[seq_len(model_count)]) <= 0) {
    return(list(type = "sl", models = list(constant_model(y, binary)), weights = 1, binary = binary))
  }

  full_models <- full_models[seq_len(model_count)]
  full_weights <- full_weights[seq_len(model_count)]
  full_weights <- full_weights / sum(full_weights)
  list(type = "sl", models = full_models, weights = full_weights, binary = binary)
}

predict_super_learner <- function(sl_fit, W_new) {
  pred_mat <- do.call(cbind, lapply(sl_fit$models, predict_base_model, W_new = W_new))
  pred <- as.vector(pred_mat %*% sl_fit$weights)
  if (sl_fit$binary) pred <- clip_prob(pred)
  pred
}

fit_nuisance <- function(dat, idx, seed) {
  outcome_candidates <- c("glm", "glmnet", "gam", "xgb2", "xgb3", "xgb5", "xgb6", "xgb8", "rf", "mars")
  propensity_candidates <- c("glm", "glmnet", "gam", "xgb2", "xgb4", "xgb6")

  W_train <- dat$W[idx, , drop = FALSE]
  q_train <- data.frame(W_train, A = dat$A[idx])
  q_fit <- fit_super_learner(
    q_train,
    dat$Y[idx],
    outcome_candidates,
    binary = TRUE,
    seed = seed + 1
  )
  g_fit <- fit_super_learner(
    W_train,
    dat$A[idx],
    propensity_candidates,
    binary = TRUE,
    seed = seed + 2
  )

  list(q_fit = q_fit, g_fit = g_fit)
}

predict_nuisance <- function(nuisance_fit, W_new) {
  W_new <- as.data.frame(W_new)
  q1 <- predict_super_learner(nuisance_fit$q_fit, data.frame(W_new, A = 1))
  q0 <- predict_super_learner(nuisance_fit$q_fit, data.frame(W_new, A = 0))
  g <- predict_super_learner(nuisance_fit$g_fit, W_new)
  list(q1 = clip_prob(q1), q0 = clip_prob(q0), g = clip_prob(g))
}

make_pseudo_outcome <- function(A, Y, q1, q0, g) {
  EY <- ifelse(A == 1, q1, q0)
  q1 - q0 + (A - g) / (g * (1 - g)) * (Y - EY)
}

fit_learner <- function(learner, W, zeta) {
  if (learner == "GAM") {
    return(fit_base_model("gam", W, zeta, binary = FALSE))
  }
  if (learner == "RF") {
    return(fit_base_model("rf", W, zeta, binary = FALSE))
  }
  if (learner == "MARS") {
    return(fit_base_model("mars", W, zeta, binary = FALSE))
  }
  if (learner == "GLMnet") {
    return(fit_base_model("glmnet", W, zeta, binary = FALSE))
  }
  if (grepl("^GBRT", learner)) {
    depth <- as.integer(sub("GBRT ", "", learner))
    return(fit_base_model(paste0("xgb", depth), W, zeta, binary = FALSE))
  }
  stop("Unknown learner: ", learner)
}

predict_learner <- function(model, learner, W_new) {
  predict_base_model(model, W_new)
}

fit_crossfit_algorithm3 <- function(dat, learner, folds) {
  n <- nrow(dat$W)
  tau_oof <- numeric(n)
  q1_oof <- numeric(n)
  q0_oof <- numeric(n)
  g_oof <- numeric(n)
  fold_models <- vector("list", length(folds))

  for (fold_id in seq_along(folds)) {
    valid <- folds[[fold_id]]
    train <- setdiff(seq_len(n), valid)

    nuisance_fit <- fit_nuisance(dat, train, seed = 50000 + 1000 * fold_id + n)
    train_nuisance <- predict_nuisance(nuisance_fit, dat$W[train, , drop = FALSE])
    valid_nuisance <- predict_nuisance(nuisance_fit, dat$W[valid, , drop = FALSE])

    zeta_train <- make_pseudo_outcome(
      dat$A[train],
      dat$Y[train],
      train_nuisance$q1,
      train_nuisance$q0,
      train_nuisance$g
    )

    model <- fit_learner(learner, dat$W[train, , drop = FALSE], zeta_train)
    tau_oof[valid] <- predict_learner(model, learner, dat$W[valid, , drop = FALSE])
    q1_oof[valid] <- valid_nuisance$q1
    q0_oof[valid] <- valid_nuisance$q0
    g_oof[valid] <- valid_nuisance$g
    fold_models[[fold_id]] <- model
  }

  list(tau_oof = tau_oof, q1_oof = q1_oof, q0_oof = q0_oof, g_oof = g_oof, fold_models = fold_models)
}

predict_fold_matrix <- function(fit, learner, W_new) {
  do.call(cbind, lapply(fit$fold_models, function(model) {
    predict_learner(model, learner, W_new)
  }))
}

collapse_fold_predictions <- function(tau_mat) {
  as.vector(apply(tau_mat, 1, quantile, type = 1, probs = 0.5))
}

fit_gamma_depth <- function(gamma_data, seed) {
  set.seed(seed)
  candidate_depths <- c(1, 2, 3, 4, 5)
  fold_id <- sample(rep(seq_len(5), length.out = nrow(gamma_data)))
  cv_risk <- sapply(candidate_depths, function(depth) {
    fold_risk <- numeric(5)
    for (fold in seq_len(5)) {
      train <- fold_id != fold
      valid <- fold_id == fold
      fit <- gbm(
        tau ~ pred,
        data = gamma_data[train, ],
        distribution = "gaussian",
        n.trees = 250,
        interaction.depth = depth,
        shrinkage = 0.03,
        bag.fraction = 0.8,
        train.fraction = 1.0,
        verbose = FALSE
      )
      gamma_hat <- predict(fit, newdata = gamma_data[valid, ], n.trees = fit$n.trees)
      fold_risk[fold] <- mean((gamma_data$tau[valid] - gamma_hat)^2)
    }
    mean(fold_risk)
  })
  candidate_depths[which.min(cv_risk)]
}

fit_gamma_model <- function(pred, truth, seed) {
  gamma_data <- data.frame(pred = as.numeric(pred), tau = as.numeric(truth))
  depth <- fit_gamma_depth(gamma_data, seed = seed)
  gbm(
    tau ~ pred,
    data = gamma_data,
    distribution = "gaussian",
    n.trees = 250,
    interaction.depth = depth,
    shrinkage = 0.03,
    bag.fraction = 0.8,
    train.fraction = 1.0,
    verbose = FALSE
  )
}

calibration_error <- function(pred, truth, gamma_hat, var_scale) {
  pred <- as.numeric(pred)
  truth <- as.numeric(truth)
  gamma_hat <- as.numeric(gamma_hat)
  max(mean((truth - pred) * (gamma_hat - pred)) / var_scale, 1e-12)
}

mse <- function(pred, truth, var_scale) {
  max(mean((as.numeric(pred) - as.numeric(truth))^2) / var_scale, 1e-12)
}

support_definitions_end <- TRUE

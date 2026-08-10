sl3_engine_config <- list(
  gbm_n_trees = 2000L,
  gbm_shrinkage = 0.005,
  rf_num_trees = 300L
)

nuisance_sl3_library_spec <- function() {
  list(
    id = "reduced_nuisance_sl_v1_q_glmnet_gbrt8_rf__g_glm_glmnet_gbrt2",
    outcome_candidates = c("glmnet", "gbrt8", "rf"),
    propensity_candidates = c("glm", "glmnet", "gbrt2")
  )
}

configure_sl3_paper_engine <- function(
  gbm_n_trees = 2000L,
  gbm_shrinkage = 0.005,
  rf_num_trees = 300L
) {
  sl3_engine_config$gbm_n_trees <<- as.integer(gbm_n_trees)
  sl3_engine_config$gbm_shrinkage <<- as.numeric(gbm_shrinkage)
  sl3_engine_config$rf_num_trees <<- as.integer(rf_num_trees)
  invisible(sl3_engine_config)
}

assert_sl3_engine_packages <- function() {
  required <- c("sl3", "origami", "glmnet", "ranger", "gbm", "nnls")
  missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0) {
    stop("Install missing sl3 engine packages first: ", paste(missing, collapse = ", "))
  }
}

sl3_dummy_outcome <- function(n, outcome_type) {
  if (outcome_type == "binomial") {
    return(as.numeric(seq_len(n) %% 2L))
  }
  seq_len(n) / max(n, 1L)
}

make_sl3_task <- function(X, y, outcome_type) {
  X <- as.data.frame(X)
  covariates <- names(X)
  dat <- data.frame(X, sl3_outcome = as.numeric(y))
  sl3::sl3_Task$new(
    data = dat,
    covariates = covariates,
    outcome = "sl3_outcome",
    outcome_type = outcome_type
  )
}

make_sl3_prediction_task <- function(model, X) {
  X <- as.data.frame(X)
  missing_covariates <- setdiff(model$covariates, names(X))
  if (length(missing_covariates) > 0) {
    stop("Prediction data are missing covariates: ", paste(missing_covariates, collapse = ", "))
  }
  X <- X[, model$covariates, drop = FALSE]
  dat <- data.frame(
    X,
    sl3_outcome = sl3_dummy_outcome(nrow(X), model$outcome_type)
  )
  sl3::sl3_Task$new(
    data = dat,
    covariates = model$covariates,
    outcome = "sl3_outcome",
    outcome_type = model$outcome_type
  )
}

clip_sl3_prob <- function(x) {
  if (exists("clip_prob", inherits = TRUE)) {
    return(clip_prob(x))
  }
  pmin(pmax(as.numeric(x), 0.01), 0.99)
}

constant_sl3_model <- function(y, outcome_type, covariates) {
  value <- mean(as.numeric(y))
  if (outcome_type == "binomial") {
    value <- clip_sl3_prob(value)
  }
  list(
    type = "constant_sl3",
    value = value,
    outcome_type = outcome_type,
    covariates = covariates
  )
}

fit_sl3_model <- function(learner, X, y, outcome_type, seed = NULL) {
  X <- as.data.frame(X)
  y <- as.numeric(y)
  covariates <- names(X)
  if (!all(is.finite(y)) || length(unique(y)) < 2L) {
    return(constant_sl3_model(y, outcome_type, covariates))
  }

  if (!is.null(seed)) {
    set.seed(seed)
  }
  task <- make_sl3_task(X, y, outcome_type)
  fit <- learner$train(task)
  list(
    type = "sl3",
    fit = fit,
    outcome_type = outcome_type,
    covariates = covariates
  )
}

predict_sl3_model <- function(model, X_new) {
  if (identical(model$type, "constant_sl3")) {
    pred <- rep(model$value, nrow(as.data.frame(X_new)))
  } else {
    task_new <- make_sl3_prediction_task(model, X_new)
    pred <- as.numeric(model$fit$predict(task_new))
  }

  finite <- is.finite(pred)
  if (!all(finite)) {
    replacement <- if (any(finite)) mean(pred[finite]) else 0
    pred[!finite] <- replacement
  }
  if (model$outcome_type == "binomial") {
    pred <- clip_sl3_prob(pred)
  }
  pred
}

sl3_gbm <- function(depth) {
  sl3::Lrnr_gbm$new(
    interaction.depth = as.integer(depth),
    n.trees = sl3_engine_config$gbm_n_trees,
    shrinkage = sl3_engine_config$gbm_shrinkage
  )
}

sl3_paper_candidate <- function(candidate) {
  if (candidate == "glm") {
    return(sl3::Lrnr_glm$new())
  }
  if (candidate == "glmnet") {
    return(sl3::Lrnr_glmnet$new())
  }
  if (candidate == "gam") {
    return(sl3::Lrnr_gam$new())
  }
  if (candidate == "rf") {
    return(sl3::Lrnr_ranger$new(num.trees = sl3_engine_config$rf_num_trees))
  }
  if (candidate == "mars") {
    return(sl3::Lrnr_earth$new())
  }
  if (grepl("^gbrt[0-9]+$", candidate)) {
    return(sl3_gbm(as.integer(sub("^gbrt", "", candidate))))
  }
  stop("Unknown sl3 candidate learner: ", candidate)
}

make_sl3_super_learner <- function(candidates) {
  if (length(candidates) == 1L) {
    return(sl3_paper_candidate(candidates[[1L]]))
  }
  stack_args <- lapply(candidates, sl3_paper_candidate)
  stack <- do.call(sl3::Stack$new, stack_args)
  sl3::Lrnr_sl$new(
    learners = stack,
    metalearner = sl3::Lrnr_nnls$new()
  )
}

fit_nuisance_sl3 <- function(dat, idx, seed, outcome_type = "binomial", dgp = "scenario1") {
  assert_sl3_engine_packages()

  if (!identical(tolower(dgp), "scenario1")) {
    stop("Only scenario1 is supported in the thesis experiments.")
  }
  library_spec <- nuisance_sl3_library_spec()
  outcome_candidates <- library_spec$outcome_candidates
  propensity_candidates <- library_spec$propensity_candidates

  W_train <- dat$W[idx, , drop = FALSE]
  q_train <- data.frame(W_train, A = dat$A[idx])

  q_fit <- fit_sl3_model(
    make_sl3_super_learner(outcome_candidates),
    q_train,
    dat$Y[idx],
    outcome_type = outcome_type,
    seed = seed + 1L
  )
  g_fit <- fit_sl3_model(
    make_sl3_super_learner(propensity_candidates),
    W_train,
    dat$A[idx],
    outcome_type = "binomial",
    seed = seed + 2L
  )

  list(q_fit = q_fit, g_fit = g_fit, library_spec = library_spec)
}

predict_nuisance_sl3 <- function(nuisance_fit, W_new) {
  W_new <- as.data.frame(W_new)
  q1 <- predict_sl3_model(nuisance_fit$q_fit, data.frame(W_new, A = 1))
  q0 <- predict_sl3_model(nuisance_fit$q_fit, data.frame(W_new, A = 0))
  g <- predict_sl3_model(nuisance_fit$g_fit, W_new)
  if (identical(nuisance_fit$q_fit$outcome_type, "binomial")) {
    q1 <- clip_sl3_prob(q1)
    q0 <- clip_sl3_prob(q0)
  }
  list(q1 = q1, q0 = q0, g = clip_sl3_prob(g))
}

fit_learner_sl3 <- function(learner, W, zeta) {
  assert_sl3_engine_packages()

  if (learner == "GAM") {
    return(fit_sl3_model(sl3::Lrnr_gam$new(), W, zeta, outcome_type = "continuous", seed = NULL))
  }
  if (learner == "RF") {
    return(fit_sl3_model(
      sl3::Lrnr_ranger$new(num.trees = sl3_engine_config$rf_num_trees),
      W,
      zeta,
      outcome_type = "continuous",
      seed = NULL
    ))
  }
  if (learner == "MARS") {
    return(fit_sl3_model(sl3::Lrnr_earth$new(), W, zeta, outcome_type = "continuous", seed = NULL))
  }
  if (learner == "GLMnet") {
    return(fit_sl3_model(sl3::Lrnr_glmnet$new(), W, zeta, outcome_type = "continuous", seed = NULL))
  }
  if (grepl("^GBRT", learner)) {
    depth <- as.integer(sub("GBRT ", "", learner))
    return(fit_sl3_model(sl3_gbm(depth), W, zeta, outcome_type = "continuous", seed = NULL))
  }
  stop("Unknown learner: ", learner)
}

predict_learner_sl3 <- function(model, learner, W_new) {
  predict_sl3_model(model, W_new)
}

activate_sl3_paper_engine <- function(opts) {
  assert_sl3_engine_packages()
  configure_sl3_paper_engine(
    gbm_n_trees = opts$sl3_gbm_trees,
    gbm_shrinkage = opts$sl3_gbm_shrinkage,
    rf_num_trees = opts$sl3_rf_trees
  )
  assign("fit_nuisance", fit_nuisance_sl3, envir = .GlobalEnv)
  assign("predict_nuisance", predict_nuisance_sl3, envir = .GlobalEnv)
  assign("fit_learner", fit_learner_sl3, envir = .GlobalEnv)
  assign("predict_learner", predict_learner_sl3, envir = .GlobalEnv)
  invisible(TRUE)
}

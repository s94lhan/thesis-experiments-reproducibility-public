simulate_dgp1 <- function(n, seed, draw_outcome = TRUE) {
  set.seed(seed)

  W <- data.frame(
    W1 = runif(n, -1, 1),
    W2 = runif(n, -1, 1),
    W3 = runif(n, -1, 1),
    W4 = runif(n, -1, 1)
  )

  qbar <- function(a, w) {
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
  q1 <- qbar(1, W)
  q0 <- qbar(0, W)
  tau <- q1 - q0

  if (draw_outcome) {
    A <- rbinom(n, 1, g)
    Y1 <- rbinom(n, 1, q1)
    Y0 <- rbinom(n, 1, q0)
    Y <- A * Y1 + (1 - A) * Y0
  } else {
    A <- rep(NA_integer_, n)
    Y1 <- rep(NA_real_, n)
    Y0 <- rep(NA_real_, n)
    Y <- rep(NA_real_, n)
  }

  list(
    W = W,
    A = A,
    Y = Y,
    Y1 = Y1,
    Y0 = Y0,
    q1 = q1,
    q0 = q0,
    g = g,
    tau = tau
  )
}

oracle_dr_pseudo_outcome <- function(dat) {
  dat$tau +
    dat$A * (dat$Y - dat$q1) / dat$g -
    (1 - dat$A) * (dat$Y - dat$q0) / (1 - dat$g)
}

generate_cal_eval_pair <- function(n, repeat_id, seed_base = 20260616L) {
  cal_seed <- seed_base + repeat_id * 1000L + 11L
  eval_seed <- seed_base + repeat_id * 1000L + 37L

  list(
    cal = simulate_dgp1(n = n, seed = cal_seed, draw_outcome = TRUE),
    eval = simulate_dgp1(n = n, seed = eval_seed, draw_outcome = TRUE)
  )
}

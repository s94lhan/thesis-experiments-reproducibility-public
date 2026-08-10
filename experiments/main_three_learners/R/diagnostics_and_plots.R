iso_learner_colors <- c(
  "GBRT 8" = "#B24C3A",
  "GLMnet" = "#007C73",
  "RF" = "#B88A00"
)

iso_policy_colors <- c(
  "Oracle true CATE" = "#C95C54",
  "Raw DR Learner" = "#007C73",
  "Raw DR learner" = "#007C73",
  "Causal Isotonic" = "#4C78A8"
)

iso_score_colors <- c(
  "True CATE" = "#C95C54",
  "Raw score" = "#007C73",
  "Raw DR learner" = "#007C73",
  "Calibrated score" = "#4C78A8",
  "Causal isotonic" = "#4C78A8"
)

iso_contribution_colors <- c(
  "positive value contribution" = "#007C73",
  "negative value contribution" = "#A3403D"
)

if (.Platform$OS.type == "windows" && !"Times New Roman" %in% names(grDevices::windowsFonts())) {
  grDevices::windowsFonts(`Times New Roman` = grDevices::windowsFont("Times New Roman"))
}

iso_paper_theme <- function(base_size = 8.5) {
  ggplot2::theme_bw(base_size = base_size) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        family = "Times New Roman",
        face = "bold",
        hjust = 0.5,
        size = 10,
        lineheight = 1.5
      ),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, size = base_size - 0.8, colour = "grey30"),
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold", hjust = 0, size = base_size, margin = ggplot2::margin(b = 3)),
      panel.border = ggplot2::element_rect(colour = "grey45", fill = NA, linewidth = 0.35),
      panel.grid.major = ggplot2::element_line(colour = "grey92", linewidth = 0.24),
      panel.grid.minor = ggplot2::element_blank(),
      axis.title = ggplot2::element_text(face = "bold", size = base_size),
      axis.text = ggplot2::element_text(colour = "grey25", size = base_size - 1),
      legend.position = "bottom",
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(size = base_size - 1),
      legend.key.width = grid::unit(0.80, "cm"),
      legend.key.height = grid::unit(0.32, "cm"),
      legend.margin = ggplot2::margin(t = 1),
      panel.spacing = grid::unit(0.75, "lines"),
      plot.margin = ggplot2::margin(5, 7, 5, 7)
    )
}

fit_cal_gamma_model <- function(pred, truth, seed, options) {
  if (options$cal_mode == "paper") {
    return(fit_gamma_model(pred, truth, seed))
  }

  set.seed(seed)
  gamma_data <- data.frame(pred = as.numeric(pred), tau = as.numeric(truth))
  gbm::gbm(
    tau ~ pred,
    data = gamma_data,
    distribution = "gaussian",
    n.trees = options$cal_gbm_trees,
    interaction.depth = options$cal_gbm_depth,
    shrinkage = 0.03,
    bag.fraction = 0.8,
    train.fraction = 1.0,
    verbose = FALSE
  )
}

iso_calibration_error <- function(pred, truth, gamma_hat) {
  value <- mean((as.numeric(truth) - as.numeric(pred)) * (as.numeric(gamma_hat) - as.numeric(pred)))
  max(value, 1e-12)
}

iso_metric_rows <- function(
  method,
  raw,
  calibrated,
  test,
  raw_gamma_hat = NULL,
  calibrated_gamma_hat = NULL
) {
  rows <- list(
    data.table::data.table(
      calibration_method = method,
      estimator = "Raw DR Learner",
      metric = "MSE",
      value = max(mean((raw - test$tau)^2), 1e-12)
    ),
    data.table::data.table(
      calibration_method = method,
      estimator = "Causal Isotonic",
      metric = "MSE",
      value = max(mean((calibrated - test$tau)^2), 1e-12)
    )
  )

  if (!is.null(raw_gamma_hat) && !is.null(calibrated_gamma_hat)) {
    rows <- c(
      rows,
      list(
        data.table::data.table(
          calibration_method = method,
          estimator = "Raw DR Learner",
          metric = "CAL",
          value = iso_calibration_error(raw, test$tau, raw_gamma_hat)
        ),
        data.table::data.table(
          calibration_method = method,
          estimator = "Causal Isotonic",
          metric = "CAL",
          value = iso_calibration_error(calibrated, test$tau, calibrated_gamma_hat)
        )
      )
    )
  }

  data.table::rbindlist(rows, use.names = TRUE)
}

policy_rows <- function(raw, calibrated, test, method, threshold) {
  oracle_rule <- test$tau > threshold
  raw_rule <- raw > threshold
  calibrated_rule <- calibrated > threshold
  oracle_value <- mean(test$Q0 + as.numeric(oracle_rule) * test$tau)

  make_row <- function(policy, rule) {
    total_value <- mean(test$Q0 + as.numeric(rule) * test$tau)
    data.table::data.table(
      calibration_method = method,
      policy = policy,
      total_policy_value = total_value,
      incremental_policy_value = mean(as.numeric(rule) * test$tau),
      regret_to_oracle = oracle_value - total_value,
      treat_rate = mean(rule),
      oracle_agreement = mean(rule == oracle_rule)
    )
  }

  data.table::rbindlist(list(
    make_row("Oracle true CATE", oracle_rule),
    make_row("Raw DR Learner", raw_rule),
    make_row("Causal Isotonic", calibrated_rule)
  ))
}

threshold_diagnostic_rows <- function(
  calibrator,
  tau_oof,
  raw,
  calibrated,
  tau0,
  method,
  threshold
) {
  raw_rule <- raw > threshold
  iso_rule <- calibrated > threshold
  flip <- raw_rule != iso_rule
  t_theta <- effective_threshold(calibrator, tau_oof, threshold)
  tolerance <- 1e-10
  direction <- if (is.na(t_theta)) {
    "undefined"
  } else if (t_theta > threshold + tolerance) {
    "conservative"
  } else if (t_theta < threshold - tolerance) {
    "aggressive"
  } else {
    "neutral"
  }

  data.table::data.table(
    calibration_method = method,
    theta_0 = as.numeric(calibrator$calibration_function(threshold)),
    t_theta = t_theta,
    threshold_direction = direction,
    flip_region_size = mean(flip),
    flip_region_mean_tau0 = if (any(flip)) mean(tau0[flip]) else NA_real_,
    DeltaV = mean((as.numeric(iso_rule) - as.numeric(raw_rule)) * tau0),
    TreatRate_raw = mean(raw_rule),
    TreatRate_iso = mean(iso_rule)
  )
}

flip_decomposition_rows <- function(raw, calibrated, tau0, method, threshold) {
  raw_rule <- raw > threshold
  iso_rule <- calibrated > threshold
  corrected_under <- !raw_rule & iso_rule & tau0 > threshold
  corrected_over <- raw_rule & !iso_rule & tau0 < threshold
  induced_over <- !raw_rule & iso_rule & tau0 < threshold
  induced_under <- raw_rule & !iso_rule & tau0 > threshold

  data.table::data.table(
    calibration_method = method,
    corrected_under_share = mean(corrected_under),
    corrected_under_value = mean(as.numeric(corrected_under) * tau0),
    corrected_over_share = mean(corrected_over),
    corrected_over_value = mean(as.numeric(corrected_over) * -tau0),
    induced_over_share = mean(induced_over),
    induced_over_value = mean(as.numeric(induced_over) * tau0),
    induced_under_share = mean(induced_under),
    induced_under_value = mean(as.numeric(induced_under) * -tau0),
    DeltaV = mean((as.numeric(iso_rule) - as.numeric(raw_rule)) * tau0)
  )
}

make_eval_prediction_rows <- function(raw, calibrated, tau0, method, threshold) {
  data.table::data.table(
    eval_id = seq_along(tau0),
    calibration_method = method,
    tau0_eval = tau0,
    tau_raw_eval = raw,
    tau_iso_eval = calibrated,
    d_raw = as.integer(raw > threshold),
    d_iso = as.integer(calibrated > threshold),
    d_oracle = as.integer(tau0 > threshold)
  )
}

summarize_metric_results <- function(dt) {
  dt[, .(
    value = mean(value),
    sd = stats::sd(value),
    mc_se = stats::sd(value) / sqrt(.N),
    repeats = .N
  ), by = .(sample_size, learner, calibration_method, estimator, metric)]
}

summarize_policy_results <- function(dt) {
  dt[, .(
    total_policy_value = mean(total_policy_value),
    incremental_policy_value = mean(incremental_policy_value),
    regret_to_oracle = mean(regret_to_oracle),
    treat_rate = mean(treat_rate),
    oracle_agreement = mean(oracle_agreement),
    total_policy_value_sd = stats::sd(total_policy_value),
    repeats = .N
  ), by = .(sample_size, learner, calibration_method, policy, threshold)]
}

summarize_threshold_diagnostics <- function(dt) {
  dt[, .(
    theta_0 = mean(theta_0),
    t_theta = if (all(is.finite(t_theta))) mean(t_theta) else NA_real_,
    flip_region_size = mean(flip_region_size),
    flip_region_mean_tau0 = mean(flip_region_mean_tau0, na.rm = TRUE),
    DeltaV = mean(DeltaV),
    TreatRate_raw = mean(TreatRate_raw),
    TreatRate_iso = mean(TreatRate_iso)
  ), by = .(sample_size, learner, calibration_method, threshold_direction)]
}

summarize_flip_decomposition <- function(dt) {
  value_columns <- setdiff(
    names(dt),
    c("sample_size", "repeat_id", "learner", "calibration_method")
  )
  dt[, lapply(.SD, mean), .SDcols = value_columns,
     by = .(sample_size, learner, calibration_method)]
}

make_raw_bin_fliprate <- function(eval_dt, bin_count = 30L) {
  eval_dt[, {
    lower <- as.numeric(stats::quantile(tau_raw_eval, 0.01, names = FALSE, na.rm = TRUE))
    upper <- as.numeric(stats::quantile(tau_raw_eval, 0.99, names = FALSE, na.rm = TRUE))
    if (!is.finite(lower) || !is.finite(upper) || upper <= lower) {
      lower <- min(tau_raw_eval, na.rm = TRUE)
      upper <- max(tau_raw_eval, na.rm = TRUE) + 1e-8
    }
    keep <- tau_raw_eval >= lower & tau_raw_eval <= upper
    local <- .SD[keep]
    breaks <- seq(lower, upper, length.out = bin_count + 1L)
    local[, raw_bin := cut(
      tau_raw_eval,
      breaks = breaks,
      include.lowest = TRUE,
      labels = FALSE
    )]
    local[, corrected := (
      (d_raw == 0L & d_iso == 1L & tau0_eval > 0) |
        (d_raw == 1L & d_iso == 0L & tau0_eval < 0)
    )]
    local[, wrong := (
      (d_raw == 0L & d_iso == 1L & tau0_eval < 0) |
        (d_raw == 1L & d_iso == 0L & tau0_eval > 0)
    )]
    local[, .(
      raw_bin_mid = mean(tau_raw_eval),
      raw_bin_lower = min(tau_raw_eval),
      raw_bin_upper = max(tau_raw_eval),
      corrected_rate = mean(corrected),
      wrong_rate = -mean(wrong),
      total_flip_rate = mean(d_raw != d_iso),
      count = .N
    ), by = raw_bin]
  }, by = .(sample_size, learner, calibration_method)]
}

save_iso_plot <- function(plot, output_path, width, height) {
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  ggplot2::ggsave(output_path, plot, width = width, height = height, dpi = 240)
  invisible(plot)
}

assign_equal_count_bins <- function(x, n_bins) {
  n <- length(x)
  ord <- order(x, na.last = TRUE)
  bins <- integer(n)
  bins[ord] <- pmin(n_bins, ceiling(seq_len(n) / n * n_bins))
  bins
}

plot_score_error_spearman_delta_v <- function(metrics_raw, threshold_dt, eval_dt, output_path) {
  learner_levels <- c("GBRT 8", "GLMnet", "RF")
  estimator_levels <- c("Raw DR Learner", "Causal Isotonic")
  estimator_linetypes <- c(
    "Raw DR Learner" = "solid",
    "Causal Isotonic" = "longdash",
    "Delta V" = "solid"
  )

  metric_summary <- data.table::copy(metrics_raw[
    calibration_method == "Causal Isotonic" &
      metric %in% c("CAL", "MSE") &
      estimator %in% estimator_levels
  ])
  metric_summary <- metric_summary[, .(
    mean = mean(value, na.rm = TRUE)
  ), by = .(sample_size, learner, estimator, metric)]

  spearman_summary <- data.table::copy(eval_dt[
    calibration_method == "Causal Isotonic" &
      is.finite(tau0_eval) &
      is.finite(tau_raw_eval) &
      is.finite(tau_iso_eval)
  ])
  spearman_summary <- spearman_summary[, .(
    `Raw DR Learner` = suppressWarnings(stats::cor(tau_raw_eval, tau0_eval, method = "spearman")),
    `Causal Isotonic` = suppressWarnings(stats::cor(tau_iso_eval, tau0_eval, method = "spearman"))
  ), by = .(sample_size, repeat_id, learner)]
  spearman_summary <- data.table::melt(
    spearman_summary,
    id.vars = c("sample_size", "repeat_id", "learner"),
    measure.vars = estimator_levels,
    variable.name = "estimator",
    value.name = "value"
  )
  spearman_summary <- spearman_summary[, .(
    mean = mean(value, na.rm = TRUE)
  ), by = .(sample_size, learner, estimator)]
  spearman_summary[, metric := "Spearman"]

  delta_summary <- data.table::copy(threshold_dt[
    calibration_method == "Causal Isotonic" &
      is.finite(DeltaV)
  ])
  delta_summary <- delta_summary[, .(
    mean = mean(DeltaV, na.rm = TRUE)
  ), by = .(sample_size, learner)]
  delta_summary[, `:=`(
    estimator = "Delta V",
    metric = "Delta V"
  )]

  plot_dt <- data.table::rbindlist(list(
    metric_summary,
    spearman_summary,
    delta_summary
  ), use.names = TRUE, fill = TRUE)
  plot_dt[, learner := factor(learner, levels = learner_levels)]
  plot_dt[, estimator := factor(estimator, levels = c(estimator_levels, "Delta V"))]
  plot_dt[, panel := factor(
    metric,
    levels = c("CAL", "MSE", "Spearman", "Delta V"),
    labels = c("A. CAL", "B. MSE", "C. Spearman rank correlation", "D. Delta V")
  )]

  p <- ggplot2::ggplot(
    plot_dt,
    ggplot2::aes(
      x = sample_size,
      y = mean,
      colour = learner,
      linetype = estimator,
      group = interaction(learner, estimator)
    )
  ) +
    ggplot2::geom_line(linewidth = 0.50, lineend = "round") +
    ggplot2::geom_point(size = 1.25) +
    ggplot2::geom_hline(
      data = data.table::data.table(panel = factor("D. Delta V", levels = levels(plot_dt$panel))),
      ggplot2::aes(yintercept = 0),
      inherit.aes = FALSE,
      colour = "grey45",
      linetype = "dashed",
      linewidth = 0.30
    ) +
    ggplot2::facet_wrap(~ panel, nrow = 2, scales = "free_y") +
    ggplot2::scale_x_continuous(
      breaks = c(1000, 2000, 5000),
      labels = c("1000", "2000", "5000")
    ) +
    ggplot2::scale_colour_manual(values = iso_learner_colors[c("GBRT 8", "GLMnet", "RF")], drop = FALSE) +
    ggplot2::scale_linetype_manual(
      values = estimator_linetypes,
      breaks = estimator_levels,
      drop = FALSE
    ) +
    ggplot2::labs(
      title = "Figure 4-1. CATE Score-Level Performance and Policy Value Changes Before and After Calibration",
      subtitle = "Lines show Monte Carlo means over 300 replications; Delta V is V(iso) - V(raw).",
      x = "sample size",
      y = "metric value",
      colour = NULL,
      linetype = NULL
    ) +
    iso_paper_theme(base_size = 8.5) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5),
      plot.subtitle = ggplot2::element_text(hjust = 0.5),
      legend.position = "bottom",
      legend.box = "vertical",
      legend.key.width = grid::unit(0.95, "cm")
    ) +
    ggplot2::guides(
      colour = ggplot2::guide_legend(order = 1, nrow = 1),
      linetype = ggplot2::guide_legend(order = 2, nrow = 1)
    )

  save_iso_plot(p, output_path, 9.2, 5.8)
}

plot_raw_score_quintile_boxplots <- function(eval_dt, output_path, preferred_n = 5000L, n_groups = 5L) {
  learner_levels <- c("GBRT 8", "GLMnet", "RF")
  selected_n <- if (preferred_n %in% eval_dt$sample_size) preferred_n else max(eval_dt$sample_size)
  local <- data.table::copy(eval_dt[
    sample_size == selected_n &
      learner %in% learner_levels &
      is.finite(tau0_eval) &
      is.finite(tau_raw_eval) &
      is.finite(tau_iso_eval)
  ])
  local[, learner_panel := factor(
    learner,
    levels = learner_levels,
    labels = c("A. GBRT 8", "B. GLMnet", "C. RF")
  )]
  local[, raw_score_group_id := assign_equal_count_bins(tau_raw_eval, n_groups),
        by = .(learner, repeat_id)]
  local[, raw_score_group := factor(
    raw_score_group_id,
    levels = seq_len(n_groups),
    labels = c("Q1\nlowest", "Q2", "Q3", "Q4", "Q5\nhighest")
  )]

  long <- data.table::melt(
    local,
    id.vars = c("sample_size", "repeat_id", "learner", "learner_panel", "eval_id", "raw_score_group_id", "raw_score_group"),
    measure.vars = c("tau0_eval", "tau_raw_eval", "tau_iso_eval"),
    variable.name = "quantity",
    value.name = "value"
  )
  long[, quantity := factor(
    quantity,
    levels = c("tau0_eval", "tau_raw_eval", "tau_iso_eval"),
    labels = c("True CATE", "Raw score", "Calibrated score")
  )]

  p <- ggplot2::ggplot(long, ggplot2::aes(x = raw_score_group, y = value, fill = quantity)) +
    ggplot2::geom_hline(yintercept = 0, color = "grey45", linetype = 2, linewidth = 0.32) +
    ggplot2::geom_boxplot(
      width = 0.66,
      position = ggplot2::position_dodge(width = 0.78),
      outlier.shape = NA,
      linewidth = 0.30
    ) +
    ggplot2::facet_wrap(~ learner_panel, nrow = 1) +
    ggplot2::coord_cartesian(ylim = c(-1.0, 0.35)) +
    ggplot2::scale_fill_manual(values = iso_score_colors[c("True CATE", "Raw score", "Calibrated score")]) +
    ggplot2::labs(
      title = "Figure 4-2. True CATE, Raw Score, and Calibrated Score by Raw-Score Quintiles",
      subtitle = paste0(
        "n = ", selected_n,
        "; within each Monte Carlo replication, evaluation samples are split into five equal-size raw-score groups; boxplots pool 300 replications."
      ),
      x = "raw-score quintile group",
      y = "value",
      fill = NULL
    ) +
    iso_paper_theme(base_size = 8.5) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5),
      plot.subtitle = ggplot2::element_text(hjust = 0.5),
      legend.position = "bottom",
      axis.text.x = ggplot2::element_text(size = 7.9),
      plot.margin = ggplot2::margin(8, 10, 6, 8)
    )

  save_iso_plot(p, output_path, 9.4, 3.85)
}

plot_threshold_treatment_two_panel <- function(threshold_dt, policy_dt, output_path) {
  learner_levels <- c("GBRT 8", "GLMnet", "RF")
  learner_values <- iso_learner_colors[learner_levels]
  threshold_local <- data.table::copy(threshold_dt[
    calibration_method == "Causal Isotonic" &
      is.finite(t_theta)
  ])
  threshold_local[, learner := factor(learner, levels = learner_levels)]
  threshold_local[, sample_size_f := factor(sample_size, levels = c(1000, 2000, 5000))]

  treat_summary <- data.table::copy(policy_dt[
    calibration_method == "Causal Isotonic" &
      policy %in% c("Raw DR Learner", "Causal Isotonic"),
    .(treated_share = mean(treat_rate, na.rm = TRUE)),
    by = .(sample_size, learner, policy)
  ])
  treat_summary[, learner := factor(learner, levels = learner_levels)]
  treat_summary[, policy_label := factor(
    policy,
    levels = c("Raw DR Learner", "Causal Isotonic"),
    labels = c("Raw policy", "Causal isotonic")
  )]
  true_rate <- policy_dt[
    calibration_method == "Causal Isotonic" &
      policy == "Oracle true CATE",
    mean(treat_rate, na.rm = TRUE)
  ]

  base_theme <- iso_paper_theme(base_size = 8.5) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0),
      legend.position = "bottom"
    )

  p_threshold <- ggplot2::ggplot(
    threshold_local,
    ggplot2::aes(x = sample_size_f, y = t_theta, fill = learner)
  ) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey45", linetype = "dashed", linewidth = 0.30) +
    ggplot2::geom_boxplot(
      ggplot2::aes(colour = learner),
      width = 0.68,
      position = ggplot2::position_dodge(width = 0.78),
      outlier.colour = "grey55",
      outlier.alpha = 0.45,
      outlier.size = 0.75,
      linewidth = 0.32,
      alpha = 0.72
    ) +
    ggplot2::stat_summary(
      fun = mean,
      geom = "point",
      shape = 21,
      fill = "white",
      colour = "grey20",
      size = 1.25,
      stroke = 0.35,
      position = ggplot2::position_dodge(width = 0.78)
    ) +
    ggplot2::scale_fill_manual(values = learner_values, drop = FALSE) +
    ggplot2::scale_colour_manual(values = learner_values, drop = FALSE) +
    ggplot2::coord_cartesian(ylim = c(-0.25, 1.50)) +
    ggplot2::labs(
      title = "A. Effective raw-score threshold",
      x = "sample size",
      y = expression(t[theta]),
      fill = NULL,
      colour = NULL
    ) +
    ggplot2::guides(fill = "none", colour = "none") +
    base_theme

  p_treat <- ggplot2::ggplot(
    treat_summary,
    ggplot2::aes(
      x = sample_size,
      y = treated_share,
      colour = learner,
      linetype = policy_label,
      group = interaction(learner, policy_label)
    )
  ) +
    ggplot2::geom_hline(
      yintercept = true_rate,
      colour = "grey25",
      linetype = "dotted",
      linewidth = 0.42
    ) +
    ggplot2::geom_line(linewidth = 0.52, lineend = "round") +
    ggplot2::geom_point(size = 1.35) +
    ggplot2::annotate(
      "text",
      x = 5050,
      y = true_rate,
      label = "True policy",
      hjust = 0,
      vjust = -0.45,
      size = 2.4,
      colour = "grey25"
    ) +
    ggplot2::scale_x_continuous(
      breaks = c(1000, 2000, 5000),
      labels = c("1000", "2000", "5000"),
      expand = ggplot2::expansion(mult = c(0.04, 0.14))
    ) +
    ggplot2::scale_y_continuous(
      limits = c(0, 0.34),
      breaks = seq(0, 0.30, by = 0.10),
      labels = function(x) paste0(round(100 * x), "%")
    ) +
    ggplot2::scale_colour_manual(values = learner_values, drop = FALSE) +
    ggplot2::scale_linetype_manual(values = c("Raw policy" = "solid", "Causal isotonic" = "longdash"), drop = FALSE) +
    ggplot2::labs(
      title = "B. Treatment-rate change",
      x = "sample size",
      y = "treated share",
      colour = NULL,
      linetype = NULL
    ) +
    ggplot2::guides(
      colour = ggplot2::guide_legend(order = 1, nrow = 1),
      linetype = ggplot2::guide_legend(order = 2, nrow = 1)
    ) +
    base_theme +
    ggplot2::theme(legend.box = "vertical", legend.key.width = grid::unit(0.95, "cm"))

  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  grDevices::png(output_path, width = 11.0, height = 4.95, units = "in", res = 300)
  grid::grid.newpage()
  grid::pushViewport(grid::viewport(layout = grid::grid.layout(
    nrow = 2,
    ncol = 2,
    widths = grid::unit(c(1.03, 1.10), "null"),
    heights = grid::unit(c(24, 1), c("pt", "null"))
  )))
  grid::grid.draw(grid::textGrob(
    "Figure 4-3. Effective Raw-Score Threshold and Treatment Rate Before and After Calibration",
    gp = grid::gpar(
      fontfamily = "Times New Roman",
      fontface = "bold",
      fontsize = 10,
      lineheight = 1.5
    ),
    vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 1:2)
  ))
  print(p_threshold, vp = grid::viewport(layout.pos.row = 2, layout.pos.col = 1))
  print(p_treat, vp = grid::viewport(layout.pos.row = 2, layout.pos.col = 2))
  grid::popViewport()
  grDevices::dev.off()
  invisible(output_path)
}

plot_raw_score_flip_value_publication <- function(
  eval_dt,
  output_path,
  preferred_n = 5000L,
  bin_count = 30L,
  summary_path = NULL
) {
  summary <- flip_bin_decomposition_table(
    eval_dt = eval_dt,
    bin_by = "raw",
    preferred_n = preferred_n,
    bin_count = bin_count
  )
  if (is.null(summary) || nrow(summary) == 0L) return(NULL)
  if (!is.null(summary_path)) {
    dir.create(dirname(summary_path), recursive = TRUE, showWarnings = FALSE)
    data.table::fwrite(summary, summary_path)
  }

  selected_n <- unique(summary$sample_size)[1L]
  denom <- eval_dt[sample_size == selected_n, .N, by = .(learner, repeat_id)][, stats::median(N, na.rm = TRUE)]
  long <- data.table::rbindlist(list(
    summary[, .(
      sample_size,
      learner,
      bin_mid,
      contribution_type = "positive value contribution",
      value_contribution = beneficial_value / denom
    )],
    summary[, .(
      sample_size,
      learner,
      bin_mid,
      contribution_type = "negative value contribution",
      value_contribution = harmful_value / denom
    )]
  ), use.names = TRUE)
  long[, learner_panel := factor(
    learner,
    levels = c("GBRT 8", "GLMnet", "RF"),
    labels = c("A. GBRT 8", "B. GLMnet", "C. RF")
  )]
  long[, contribution_type := factor(
    contribution_type,
    levels = c("positive value contribution", "negative value contribution")
  )]

  p <- ggplot2::ggplot(long, ggplot2::aes(x = bin_mid, y = value_contribution, colour = contribution_type)) +
    ggplot2::annotate(
      "rect",
      xmin = -0.05,
      xmax = 0.05,
      ymin = -Inf,
      ymax = Inf,
      fill = "#F0E442",
      alpha = 0.24
    ) +
    ggplot2::geom_hline(yintercept = 0, color = "grey45", linewidth = 0.30) +
    ggplot2::geom_line(linewidth = 0.50, lineend = "round") +
    ggplot2::geom_point(size = 1.05) +
    ggplot2::facet_wrap(~ learner_panel, nrow = 1, scales = "free_x") +
    ggplot2::scale_colour_manual(values = iso_contribution_colors) +
    ggplot2::labs(
      title = paste0(
        "Figure 4-4. Policy-value contributions of beneficial and harmful flips, n = ",
        selected_n
      ),
      x = "raw CATE score",
      y = "policy-value contribution per bin",
      colour = NULL
    ) +
    iso_paper_theme(base_size = 8.5) +
    ggplot2::theme(
      legend.position = "bottom",
      plot.margin = ggplot2::margin(8, 10, 6, 8)
    )

  save_iso_plot(p, output_path, 9.4, 3.25)
}

flip_bin_decomposition_table <- function(
  eval_dt,
  bin_by = c("raw", "true"),
  preferred_n = 5000L,
  bin_count = 30L
) {
  bin_by <- match.arg(bin_by)
  selected_n <- if (preferred_n %in% eval_dt$sample_size) preferred_n else max(eval_dt$sample_size)
  local <- data.table::copy(eval_dt[sample_size == selected_n])
  if (nrow(local) == 0L) {
    warning("No evaluation predictions found for flip-bin decomposition.")
    return(NULL)
  }

  score_col <- if (bin_by == "raw") "tau_raw_eval" else "tau0_eval"
  local[, bin_score := as.numeric(get(score_col))]
  local[, beneficial_flip := d_raw != d_iso & d_iso == d_oracle]
  local[, harmful_flip := d_raw != d_iso & d_iso != d_oracle]
  local[, flip_value := (as.numeric(d_iso) - as.numeric(d_raw)) * tau0_eval]

  out <- data.table::rbindlist(lapply(unique(local$learner), function(learner_name) {
    learner_dt <- local[learner == learner_name & is.finite(bin_score)]
    learner_dt[, repeat_n := .N, by = .(sample_size, repeat_id, learner)]
    breaks <- unique(as.numeric(stats::quantile(
      learner_dt$bin_score,
      probs = seq(0, 1, length.out = bin_count + 1L),
      names = FALSE,
      na.rm = TRUE
    )))
    if (length(breaks) < 3L) {
      warning("Not enough unique bin breaks for ", learner_name, " / ", bin_by)
      return(NULL)
    }
    learner_dt[, bin_id := cut(
      bin_score,
      breaks = breaks,
      include.lowest = TRUE,
      labels = FALSE
    )]
    learner_dt <- learner_dt[!is.na(bin_id)]
    if (nrow(learner_dt) == 0L) return(NULL)

    per_repeat <- learner_dt[, .(
      bin_lower = breaks[bin_id[1L]],
      bin_upper = breaks[bin_id[1L] + 1L],
      bin_mid = mean(bin_score, na.rm = TRUE),
      bin_n = .N,
      beneficial_count = sum(beneficial_flip),
      harmful_count = sum(harmful_flip),
      beneficial_rate = mean(beneficial_flip),
      harmful_rate = mean(harmful_flip),
      total_flip_rate = mean(beneficial_flip | harmful_flip),
      beneficial_value = sum(flip_value[beneficial_flip]),
      harmful_value = sum(flip_value[harmful_flip]),
      net_value = sum(flip_value)
    ), by = .(sample_size, repeat_id, learner, bin_id)]

    per_repeat[, .(
      bin_lower = mean(bin_lower),
      bin_upper = mean(bin_upper),
      bin_mid = mean(bin_mid),
      bin_n = mean(bin_n),
      beneficial_count = mean(beneficial_count),
      harmful_count = mean(harmful_count),
      beneficial_rate = mean(beneficial_rate),
      harmful_rate = mean(harmful_rate),
      total_flip_rate = mean(total_flip_rate),
      beneficial_value = mean(beneficial_value),
      harmful_value = mean(harmful_value),
      net_value = mean(net_value),
      repeats = .N
    ), by = .(sample_size, learner, bin_id)]
  }), use.names = TRUE)

  if (nrow(out) == 0L) return(NULL)
  out[, bin_by := bin_by]
  out[]
}



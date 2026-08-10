library(data.table)
library(ggplot2)

paper_policy_colors <- c(
  "Oracle true CATE" = "#C95C54",
  "True CATE" = "#C95C54",
  "Raw policy" = "#007C73",
  "Raw score" = "#007C73",
  "Causal isotonic" = "#4C78A8",
  "Calibrated score" = "#4C78A8",
  "effective threshold" = "#B88A00",
  "beneficial value" = "#007C73",
  "harmful value" = "#A3403D",
  "net Delta V" = "#333333",
  "beneficial flip rate" = "#007C73",
  "harmful flip rate" = "#A3403D",
  "beneficial flips" = "#007C73",
  "harmful flips" = "#A3403D",
  "truly should be treated" = "#007C73",
  "truly should not be treated" = "#A3403D",
  "P(tau0 > 0 | F+)" = "#007C73",
  "P(tau0 <= 0 | F-)" = "#007C73",
  "Delta MSE" = "#B24C3A",
  "Delta CAL" = "#007C73",
  "Delta V" = "#4C78A8"
)

theme_mechanism <- function(base_size = 8.5) {
  theme_bw(base_size = base_size) +
    theme(
      plot.title = element_text(
        family = "Times New Roman",
        face = "bold",
        hjust = 0.5,
        size = 10,
        lineheight = 1.5
      ),
      plot.subtitle = element_text(hjust = 0.5, size = base_size - 0.8, colour = "grey30"),
      plot.caption = element_text(hjust = 0, size = base_size - 2, colour = "grey25"),
      strip.background = element_blank(),
      strip.text = element_text(face = "bold", hjust = 0, size = base_size, margin = margin(b = 3)),
      panel.border = element_rect(colour = "grey45", fill = NA, linewidth = 0.35),
      panel.grid.major = element_line(colour = "grey92", linewidth = 0.24),
      panel.grid.minor = element_blank(),
      axis.title = element_text(face = "bold", size = base_size),
      axis.text = element_text(colour = "grey25", size = base_size - 1),
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.text = element_text(size = base_size - 1),
      legend.key.width = grid::unit(0.80, "cm"),
      legend.key.height = grid::unit(0.32, "cm"),
      legend.margin = margin(t = 1),
      panel.spacing = grid::unit(0.75, "lines"),
      plot.margin = margin(5, 7, 5, 7)
    )
}

save_plot <- function(plot, filename, width = 12, height = 7, dpi = 180) {
  ggsave(filename, plot, width = width, height = height, dpi = dpi, bg = "white")
  invisible(filename)
}

read_result <- function(opts, name) {
  results_dir <- opts$results_dir
  if (is.null(results_dir)) {
    results_dir <- file.path(opts$project_root, "runs", opts$run_id, "results")
  }
  path <- file.path(results_dir, name)
  if (!file.exists(path)) stop("Missing result file: ", path)
  fread(path)
}

policy_long <- function(dt, x_col) {
  melt(
    dt,
    id.vars = x_col,
    measure.vars = c("v_oracle", "v_raw", "v_iso"),
    variable.name = "policy",
    value.name = "policy_value"
  )[
    ,
    policy := factor(
      policy,
      levels = c("v_oracle", "v_raw", "v_iso"),
      labels = c("Oracle true CATE", "Raw policy", "Causal isotonic")
    )
  ]
}

flip_long <- function(dt, x_col) {
  melt(
    dt,
    id.vars = x_col,
    measure.vars = c("beneficial_value", "harmful_value", "delta_v"),
    variable.name = "component",
    value.name = "value"
  )[
    ,
    component := factor(
      component,
      levels = c("beneficial_value", "harmful_value", "delta_v"),
      labels = c("beneficial value", "harmful value", "net Delta V")
    )
  ]
}

flip_rate_long <- function(dt, x_col) {
  melt(
    dt,
    id.vars = x_col,
    measure.vars = c("beneficial_flip_rate", "harmful_flip_rate"),
    variable.name = "component",
    value.name = "rate"
  )[
    ,
    `:=`(
      component = factor(
        component,
        levels = c("beneficial_flip_rate", "harmful_flip_rate"),
        labels = c("beneficial flip rate", "harmful flip rate")
      ),
      signed_rate = fifelse(component == "harmful flip rate", -rate, rate)
    )
  ]
}

flip_count_long <- function(dt, x_col) {
  melt(
    dt,
    id.vars = x_col,
    measure.vars = c("beneficial_flip_count", "harmful_flip_count"),
    variable.name = "component",
    value.name = "count"
  )[
    ,
    `:=`(
      component = factor(
        component,
        levels = c("beneficial_flip_count", "harmful_flip_count"),
        labels = c("beneficial flips", "harmful flips")
      ),
      signed_count = fifelse(component == "harmful flips", -count, count)
    )
  ]
}

threshold_treat_long <- function(dt, x_col) {
  melt(
    dt,
    id.vars = x_col,
    measure.vars = c("t_theta", "treat_rate_oracle", "treat_rate_raw", "treat_rate_iso"),
    variable.name = "measure",
    value.name = "value"
  )[
    ,
    measure := factor(
      measure,
      levels = c("t_theta", "treat_rate_oracle", "treat_rate_raw", "treat_rate_iso"),
      labels = c("effective threshold", "oracle treatment rate", "raw treatment rate", "iso treatment rate")
    )
  ]
}

treated_composition_long <- function(dt, x_col) {
  melt(
    dt,
    id.vars = x_col,
    measure.vars = c("raw_treated_true_positive_share", "raw_treated_false_positive_share",
                     "iso_treated_true_positive_share", "iso_treated_false_positive_share"),
    variable.name = "component",
    value.name = "share"
  )[
    ,
    `:=`(
      policy = fifelse(grepl("^raw_", component), "Raw policy", "Causal isotonic"),
      group = fifelse(grepl("true_positive", component),
                      "truly should be treated",
                      "truly should not be treated")
    )
  ]
}

make_slope_error_policy_plot <- function(dt, x_col, x_lab, title) {
  pdt <- rbindlist(list(
    data.table(
      x = dt[[x_col]],
      metric = "Delta MSE",
      value = dt$mse_raw - dt$mse_iso
    ),
    data.table(
      x = dt[[x_col]],
      metric = "Delta CAL",
      value = dt$cal_raw - dt$cal_iso
    ),
    data.table(
      x = dt[[x_col]],
      metric = "Delta V",
      value = dt$v_iso - dt$v_raw
    )
  ))[
    ,
    `:=`(
      x_label = factor(sprintf("%.2f", x), levels = sprintf("%.2f", sort(unique(dt[[x_col]])))),
      metric = factor(metric, levels = c("Delta MSE", "Delta CAL", "Delta V"))
    )
  ]

  ggplot(pdt, aes(x = x_label, y = value, fill = metric, colour = metric)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey55", linewidth = 0.35) +
    geom_boxplot(
      position = position_dodge(width = 0.75),
      width = 0.58,
      linewidth = 0.35,
      outlier.size = 0.55,
      outlier.alpha = 0.45
    ) +
    scale_x_discrete(drop = TRUE) +
    scale_fill_manual(values = paper_policy_colors[c("Delta MSE", "Delta CAL", "Delta V")]) +
    scale_colour_manual(values = c("Delta MSE" = "#7A3A2F",
                                   "Delta CAL" = "#00564F",
                                   "Delta V" = "#2F527D")) +
    labs(
      title = title,
      x = x_lab,
      y = "Change after calibration",
      fill = NULL,
      colour = NULL,
      caption = paste0(
        "Notes: Delta MSE and Delta CAL are defined as raw minus calibrated error. ",
        "Positive values indicate lower score-level error after calibration. ",
        "Delta V is defined as V(iso) minus V(raw). ",
        "Each box summarizes 300 Monte Carlo replications."
      )
    ) +
    theme_mechanism(base_size = 8.5) +
    theme(
      legend.key.width = unit(0.9, "cm"),
      panel.spacing.x = unit(1.2, "lines")
    )
}

summarise_ci_by <- function(dt, by_cols, value_col) {
  dt[
    ,
    .(
      mean = mean(get(value_col), na.rm = TRUE),
      lo = as.numeric(stats::quantile(get(value_col), 0.025, na.rm = TRUE, names = FALSE)),
      hi = as.numeric(stats::quantile(get(value_col), 0.975, na.rm = TRUE, names = FALSE))
    ),
    by = by_cols
  ]
}

make_sorting_policy_composition_plot <- function(detail) {
  required <- c(
    "sigma", "v_raw", "v_iso",
    "raw_treated_true_positive_share", "raw_treated_false_positive_share",
    "iso_treated_true_positive_share", "iso_treated_false_positive_share"
  )
  missing <- setdiff(required, names(detail))
  if (length(missing) > 0) {
    stop("Missing sorting policy/composition column(s): ", paste(missing, collapse = ", "))
  }

  policy_long_dt <- melt(
    detail,
    id.vars = "sigma",
    measure.vars = c("v_raw", "v_iso"),
    variable.name = "series_raw",
    value.name = "value"
  )[
    ,
    `:=`(
      panel = "A. Policy value",
      policy = factor(
        fifelse(series_raw == "v_raw", "Raw policy", "Causal isotonic"),
        levels = c("Raw policy", "Causal isotonic")
      ),
      component = factor("policy value", levels = c("policy value"))
    )
  ][, .(sigma, panel, policy, component, value)]

  composition_long_dt <- melt(
    detail,
    id.vars = "sigma",
    measure.vars = c(
      "raw_treated_true_positive_share",
      "raw_treated_false_positive_share",
      "iso_treated_true_positive_share",
      "iso_treated_false_positive_share"
    ),
    variable.name = "series_raw",
    value.name = "value"
  )[
    ,
    `:=`(
      panel = "B. Treated-set composition",
      policy = factor(
        fifelse(grepl("^raw_", series_raw), "Raw policy", "Causal isotonic"),
        levels = c("Raw policy", "Causal isotonic")
      ),
      component = factor(
        fifelse(grepl("true_positive", series_raw), "truly should be treated", "truly should not be treated"),
        levels = c("policy value", "truly should be treated", "truly should not be treated")
      )
    )
  ][, .(sigma, panel, policy, component, value)]

  plot_dt <- rbindlist(list(policy_long_dt, composition_long_dt), use.names = TRUE)
  summary_dt <- summarise_ci_by(plot_dt, c("sigma", "panel", "policy", "component"), "value")
  summary_dt[
    ,
    `:=`(
      panel = factor(panel, levels = c("A. Policy value", "B. Treated-set composition")),
      policy = factor(policy, levels = c("Raw policy", "Causal isotonic")),
      component = factor(component, levels = c("policy value", "truly should be treated", "truly should not be treated"))
    )
  ]
  y_anchor_dt <- data.table(
    sigma = rep(min(summary_dt$sigma, na.rm = TRUE), 4L),
    panel = factor(
      c("A. Policy value", "A. Policy value", "B. Treated-set composition", "B. Treated-set composition"),
      levels = c("A. Policy value", "B. Treated-set composition")
    ),
    value = c(0.958, 0.967, -0.04, 1.04)
  )

  ggplot(summary_dt, aes(x = sigma, y = mean, colour = policy, fill = policy, group = interaction(policy, component))) +
    geom_blank(
      data = y_anchor_dt,
      aes(x = sigma, y = value),
      inherit.aes = FALSE
    ) +
    geom_ribbon(
      data = summary_dt[as.character(panel) == "A. Policy value"],
      aes(ymin = lo, ymax = hi),
      alpha = 0.13,
      colour = NA
    ) +
    geom_line(aes(linetype = component), linewidth = 0.44, lineend = "round") +
    geom_point(size = 1.0) +
    facet_wrap(~ panel, scales = "free_y", nrow = 1) +
    scale_x_continuous(breaks = sort(unique(summary_dt$sigma))) +
    scale_colour_manual(values = paper_policy_colors[c("Raw policy", "Causal isotonic")]) +
    scale_fill_manual(values = paper_policy_colors[c("Raw policy", "Causal isotonic")]) +
    scale_linetype_manual(
      values = c(
        "policy value" = "solid",
        "truly should be treated" = "solid",
        "truly should not be treated" = "dashed"
      )
    ) +
    labs(
      title = "Figure 4-5. Ranking-error experiment: policy value and treated-set quality",
      x = "ranking-noise strength sigma",
      y = NULL,
      colour = NULL,
      fill = NULL,
      linetype = NULL
    ) +
    theme_mechanism(base_size = 8.5) +
    theme(
      legend.position = "bottom",
      legend.key.size = grid::unit(0.35, "lines"),
      plot.margin = margin(5, 7, 4, 5)
    )
}

select_values_for_distribution <- function(x, zero_value = 0) {
  x <- sort(unique(x))
  pick <- unique(c(min(x), zero_value, max(x)))
  pick[pick %in% x]
}

subsample_for_density <- function(dt, max_reps = 20, max_per_repeat = 100) {
  setDT(dt)
  dt[
    ,
    .SD[seq(1L, .N, length.out = min(.N, max_per_repeat))],
    by = .(repeat_id, scenario, parameter_value, effective_threshold)
  ][repeat_id <= max_reps]
}

build_distribution_rows <- function(eval_data, score_raw, score_iso, repeat_id,
                                    scenario, parameter_value, effective_threshold) {
  data.table(
    repeat_id = repeat_id,
    scenario = scenario,
    parameter_value = parameter_value,
    effective_threshold = effective_threshold,
    true_cate = eval_data$tau,
    raw_score = score_raw,
    iso_score = score_iso
  )
}

distribution_long <- function(dt) {
  melt(
    dt,
    id.vars = c("repeat_id", "scenario", "parameter_value", "effective_threshold"),
    measure.vars = c("true_cate", "raw_score", "iso_score"),
    variable.name = "score_type",
    value.name = "score"
  )[
    ,
    score_type := factor(
      score_type,
      levels = c("true_cate", "raw_score", "iso_score"),
      labels = c("True CATE", "Raw score", "Causal isotonic")
    )
  ]
}

make_distribution_plot <- function(dt, title, parameter_lab) {
  pdt <- distribution_long(dt)
  pdt[, a_value := as.numeric(sub("a = ", "", parameter_value))]
  panel_map <- unique(pdt[, .(parameter_value, a_value)])
  setorder(panel_map, a_value)
  panel_map[
    ,
    `:=`(
      bias_label = fifelse(
        a_value < -1e-12,
        "negative bias",
        fifelse(a_value > 1e-12, "positive bias", "no bias")
      ),
      panel_label = sprintf(
        "%s. a = %.2f (%s)",
        LETTERS[seq_len(.N)],
        a_value,
        fifelse(
          a_value < -1e-12,
          "negative bias",
          fifelse(a_value > 1e-12, "positive bias", "no bias")
        )
      )
    )
  ]
  pdt <- merge(pdt, panel_map[, .(parameter_value, panel_label)], by = "parameter_value")
  pdt[
    ,
    score_label := factor(
      fifelse(score_type == "Causal isotonic", "Calibrated score", as.character(score_type)),
      levels = c("True CATE", "Raw score", "Calibrated score")
    )
  ]

  bands <- unique(dt[, .(scenario, parameter_value, effective_threshold)])
  bands[, a_value := as.numeric(sub("a = ", "", parameter_value))]
  bands <- merge(bands, panel_map[, .(parameter_value, panel_label)], by = "parameter_value")
  bands <- bands[is.finite(effective_threshold)]
  threshold_summary <- bands[
    ,
    .(t_theta = stats::median(effective_threshold, na.rm = TRUE)),
    by = .(parameter_value, panel_label, a_value)
  ]
  shade <- threshold_summary[is.finite(t_theta) & abs(a_value) > 1e-12 & abs(t_theta) > 1e-12][
    ,
    `:=`(
      xmin = pmin(0, t_theta),
      xmax = pmax(0, t_theta),
      x_mid = 0.5 * (pmin(0, t_theta) + pmax(0, t_theta)),
      arrow_x0 = pmin(0, t_theta) + 0.05 * abs(t_theta),
      arrow_x1 = pmax(0, t_theta) - 0.05 * abs(t_theta),
      y_arrow = 4.25,
      y_text = 4.62,
      flip_label = fifelse(t_theta < 0, "Added to treatment\nby calibration", "Removed from treatment\nby calibration")
    )
  ]

  ggplot(pdt, aes(x = score, colour = score_label, fill = score_label)) +
    geom_rect(
      data = shade,
      aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
      inherit.aes = FALSE,
      fill = "#FFF2A8",
      alpha = 0.42
    ) +
    geom_density(linewidth = 0.50, alpha = 0.07, adjust = 1.1) +
    geom_vline(xintercept = 0, linetype = "dashed", colour = "grey45", linewidth = 0.35) +
    geom_vline(
      data = threshold_summary,
      aes(xintercept = t_theta),
      inherit.aes = FALSE,
      linetype = "dashed",
      colour = "grey15",
      linewidth = 0.35
    ) +
    geom_segment(
      data = shade,
      aes(x = arrow_x0, xend = arrow_x1, y = y_arrow, yend = y_arrow),
      inherit.aes = FALSE,
      colour = paper_policy_colors[["effective threshold"]],
      linewidth = 0.36,
      arrow = grid::arrow(length = grid::unit(0.12, "cm"), ends = "both", type = "closed")
    ) +
    geom_text(
      data = shade,
      aes(x = x_mid, y = y_text, label = flip_label),
      inherit.aes = FALSE,
      colour = paper_policy_colors[["effective threshold"]],
      fontface = "italic",
      size = 2.35,
      lineheight = 0.9
    ) +
    facet_wrap(~ panel_label, nrow = 1) +
    scale_colour_manual(values = paper_policy_colors[c("True CATE", "Raw score", "Calibrated score")]) +
    scale_fill_manual(values = paper_policy_colors[c("True CATE", "Raw score", "Calibrated score")]) +
    labs(
      title = title,
      x = "Score value",
      y = "Density",
      colour = NULL,
      fill = NULL,
      caption = paste0(
        "Notes: Curves are kernel densities based on the simulated evaluation samples. ",
        "The raw score is constructed as S_raw = tau_0(W) + a.\n",
        "The grey dashed line marks the original zero threshold; the black dashed line marks the median calibrated effective threshold t_theta in the raw-score space.\n",
        "The shaded region shows the raw-score interval in which calibration changes treatment assignment."
      )
    ) +
    coord_cartesian(xlim = c(-1.25, 0.65), ylim = c(0, 5.8), expand = FALSE) +
    theme_mechanism(base_size = 8.5) +
    theme(
      legend.key.width = unit(1.0, "cm"),
      plot.margin = margin(8, 10, 6, 8)
    )
}

collect_sorting_distribution <- function(opts, max_reps = 20) {
  levels <- select_values_for_distribution(opts$sorting_sigmas, zero_value = 0)
  rows <- vector("list", length(levels) * min(opts$reps, max_reps))
  k <- 0L
  for (r in seq_len(min(opts$reps, max_reps))) {
    dat <- generate_cal_eval_pair(n = opts$n, repeat_id = r, seed_base = opts$seed_base)
    for (sigma in levels) {
      score_cal <- construct_sorting_score(dat$cal$tau, sigma = sigma, seed = stable_seed(opts$seed_base, r, sigma * 1000))
      score_eval <- construct_sorting_score(dat$eval$tau, sigma = sigma, seed = stable_seed(opts$seed_base + 17, r, sigma * 1000))
      cal <- fit_oracle_calibrator(dat$cal, score_cal)
      score_iso <- predict_iso(cal, score_eval)
      t_theta <- effective_threshold(cal, score_eval)
      k <- k + 1L
      rows[[k]] <- build_distribution_rows(
        dat$eval, score_eval, score_iso, r, "global ranking noise",
        sprintf("sigma = %.2f", sigma), t_theta
      )
    }
  }
  subsample_for_density(rbindlist(rows), max_reps = max_reps)
}

collect_additive_distribution <- function(opts, max_reps = 20) {
  levels <- select_values_for_distribution(opts$additive_grid, zero_value = 0)
  rows <- vector("list", length(levels) * min(opts$reps, max_reps))
  k <- 0L
  for (r in seq_len(min(opts$reps, max_reps))) {
    dat <- generate_cal_eval_pair(n = opts$n, repeat_id = r, seed_base = opts$seed_base)
    for (a in levels) {
      score_cal <- construct_additive_bias_score(dat$cal$tau, a)
      score_eval <- construct_additive_bias_score(dat$eval$tau, a)
      cal <- fit_oracle_calibrator(dat$cal, score_cal)
      score_iso <- predict_iso(cal, score_eval)
      t_theta <- effective_threshold(cal, score_eval)
      k <- k + 1L
      rows[[k]] <- build_distribution_rows(
        dat$eval, score_eval, score_iso, r, "additive bias",
        sprintf("a = %.2f", a), t_theta
      )
    }
  }
  subsample_for_density(rbindlist(rows), max_reps = max_reps)
}

collect_slope_distribution <- function(opts, max_reps = 20) {
  levels <- select_values_for_distribution(opts$slope_grid, zero_value = 1)
  rows <- vector("list", length(levels) * min(opts$reps, max_reps))
  k <- 0L
  for (r in seq_len(min(opts$reps, max_reps))) {
    dat <- generate_cal_eval_pair(n = opts$n, repeat_id = r, seed_base = opts$seed_base)
    for (b in levels) {
      score_cal <- construct_slope_bias_score(dat$cal$tau, a = 0, b = b)
      score_eval <- construct_slope_bias_score(dat$eval$tau, a = 0, b = b)
      cal <- fit_oracle_calibrator(dat$cal, score_cal)
      score_iso <- predict_iso(cal, score_eval)
      t_theta <- effective_threshold(cal, score_eval)
      k <- k + 1L
      rows[[k]] <- build_distribution_rows(
        dat$eval, score_eval, score_iso, r, "slope bias",
        sprintf("b = %.2f", b), t_theta
      )
    }
  }
  subsample_for_density(rbindlist(rows), max_reps = max_reps)
}

make_sorting_additive_crosscheck_four_panel <- function(dt, a_value) {
  required <- c(
    "repeat_id",
    "sigma", "a", "t_theta", "beneficial_sign_share",
    "v_oracle", "v_raw", "v_iso"
  )
  missing <- setdiff(required, names(dt))
  if (length(missing) > 0) {
    stop("Missing sorting x additive-bias panel column(s): ", paste(missing, collapse = ", "))
  }

  pdt <- copy(dt)[abs(a - a_value) < 1e-8]
  if (nrow(pdt) == 0L) {
    stop("No sorting x additive-bias rows found for a = ", a_value)
  }

  mmean <- function(x) {
    x <- x[is.finite(x)]
    if (length(x) == 0L) return(NA_real_)
    mean(x)
  }

  sigma_levels <- sort(unique(pdt$sigma))
  pdt[, sigma_label := factor(sprintf("%.2f", sigma), levels = sprintf("%.2f", sigma_levels))]

  panel_levels <- c(
    "A. Effective raw-score threshold",
    "B. Flip-set quality",
    "C. True policy value"
  )

  summarize_line <- function(data, cols, labels, panel_name) {
    out <- melt(
      data,
      id.vars = c("sigma", "sigma_label"),
      measure.vars = cols,
      variable.name = "series",
      value.name = "value"
    )[
      ,
      `:=`(
        panel = factor(panel_name, levels = panel_levels),
        series = factor(series, levels = cols, labels = labels)
      )
    ][
      ,
      .(
        value = mmean(value)
      ),
      by = .(sigma, sigma_label, panel, series)
    ]
    setorder(out, sigma, series)
    out
  }

  threshold_box_dt <- pdt[
    is.finite(t_theta),
    .(
      sigma,
      sigma_label,
      panel = factor("A. Effective raw-score threshold", levels = panel_levels),
      series = "effective threshold",
      value = t_theta
    )
  ]

  flip_label <- if (a_value < 0) {
    "P(tau0 > 0 | F+)"
  } else {
    "P(tau0 <= 0 | F-)"
  }
  flip_dt <- summarize_line(
    pdt,
    "beneficial_sign_share",
    flip_label,
    "B. Flip-set quality"
  )

  policy_dt <- summarize_line(
    pdt,
    c("v_oracle", "v_raw", "v_iso"),
    c("Oracle true CATE", "Raw policy", "Causal isotonic"),
    "C. True policy value"
  )

  line_dt <- rbindlist(
    list(flip_dt, policy_dt),
    use.names = TRUE,
    fill = TRUE
  )
  line_dt_plot <- line_dt
  first_sigma <- factor(sprintf("%.2f", min(sigma_levels)), levels = levels(pdt$sigma_label))
  threshold_range <- range(c(threshold_box_dt$value, 0, a_value), finite = TRUE)
  threshold_span <- diff(threshold_range)
  if (!is.finite(threshold_span) || threshold_span <= 0) threshold_span <- 0.05
  policy_range <- range(policy_dt$value, finite = TRUE)
  policy_span <- diff(policy_range)
  if (!is.finite(policy_span) || policy_span <= 0) policy_span <- 0.002

  flip_quality_range <- if (a_value < 0) c(0.25, 1.00) else c(0.75, 1.00)

  range_blank <- rbindlist(
    list(
      data.table(
        sigma_label = first_sigma,
        panel = factor("A. Effective raw-score threshold", levels = panel_levels),
        value = c(
          threshold_range[1] - pmax(0.25 * threshold_span, 0.015),
          threshold_range[2] + pmax(0.25 * threshold_span, 0.015)
        )
      ),
      data.table(
        sigma_label = first_sigma,
        panel = factor("B. Flip-set quality", levels = panel_levels),
        value = flip_quality_range
      ),
      data.table(
        sigma_label = first_sigma,
        panel = factor("C. True policy value", levels = panel_levels),
        value = c(
          policy_range[1] - pmax(0.55 * policy_span, 0.002),
          policy_range[2] + pmax(0.55 * policy_span, 0.002)
        )
      )
    )
  )

  ref_dt <- data.table(
    panel = factor(
      c("A. Effective raw-score threshold", "A. Effective raw-score threshold"),
      levels = panel_levels
    ),
    yintercept = c(0, a_value),
    ref = factor(
      c("raw threshold: 0", "ideal shift: a"),
      levels = c("raw threshold: 0", "ideal shift: a")
    )
  )

  a_label <- sprintf(
    "a = %+.2f (%s)",
    a_value,
    ifelse(a_value > 0, "over-treatment case", "under-treatment case")
  )
  figure_title <- if (a_value < 0) {
    "Figure 4-8. Sorting-Error Cross-Check under Negative Additive Bias (a = -0.10)"
  } else {
    "Figure 4-9. Sorting-Error Cross-Check under Positive Additive Bias (a = 0.10)"
  }

  ggplot() +
    geom_blank(
      data = range_blank,
      aes(x = sigma_label, y = value),
      inherit.aes = FALSE
    ) +
    geom_hline(
      data = ref_dt[ref == "raw threshold: 0"],
      aes(yintercept = yintercept),
      inherit.aes = FALSE,
      colour = "grey45",
      linetype = "dashed",
      linewidth = 0.35
    ) +
    geom_hline(
      data = ref_dt[ref == "ideal shift: a"],
      aes(yintercept = yintercept),
      inherit.aes = FALSE,
      colour = "grey55",
      linetype = "dotted",
      linewidth = 0.35
    ) +
    geom_boxplot(
      data = threshold_box_dt,
      aes(x = sigma_label, y = value, fill = series),
      inherit.aes = FALSE,
      width = 0.46,
      linewidth = 0.32,
      colour = "#7A5A00",
      outlier.colour = "grey55",
      outlier.size = 0.55,
      outlier.alpha = 0.6,
      show.legend = FALSE
    ) +
    geom_line(
      data = line_dt_plot,
      aes(x = sigma_label, y = value, colour = series, group = series),
      linewidth = 0.50,
      na.rm = TRUE
    ) +
    geom_point(
      data = line_dt_plot,
      aes(x = sigma_label, y = value, colour = series, group = series),
      size = 1.15,
      na.rm = TRUE
    ) +
    facet_wrap(~ panel, scales = "free_y", nrow = 1) +
    scale_x_discrete(drop = FALSE) +
    scale_colour_manual(
      values = c(
        "Oracle true CATE" = paper_policy_colors[["Oracle true CATE"]],
        "Raw policy" = paper_policy_colors[["Raw policy"]],
        "Causal isotonic" = paper_policy_colors[["Causal isotonic"]],
        "P(tau0 > 0 | F+)" = paper_policy_colors[["P(tau0 > 0 | F+)"]],
        "P(tau0 <= 0 | F-)" = paper_policy_colors[["P(tau0 <= 0 | F-)"]]
      )
    ) +
    scale_fill_manual(
      values = c(
        "effective threshold" = paper_policy_colors[["effective threshold"]],
        "Oracle true CATE" = paper_policy_colors[["Oracle true CATE"]],
        "Raw policy" = paper_policy_colors[["Raw policy"]],
        "Causal isotonic" = paper_policy_colors[["Causal isotonic"]],
        "P(tau0 > 0 | F+)" = paper_policy_colors[["P(tau0 > 0 | F+)"]],
        "P(tau0 <= 0 | F-)" = paper_policy_colors[["P(tau0 <= 0 | F-)"]]
      )
    ) +
    labs(
      title = figure_title,
      subtitle = "Panel A shows Monte Carlo distributions of effective thresholds; Panels B and C show Monte Carlo means.",
      x = "ranking-noise strength sigma",
      y = NULL,
      colour = NULL,
      fill = NULL
    ) +
    theme_mechanism(base_size = 8.5) +
    theme(
      axis.title.x = element_text(face = "bold", size = 8.2, margin = margin(t = 5)),
      legend.key.width = unit(0.75, "cm"),
      plot.margin = margin(5, 8, 5, 7),
      panel.spacing = unit(0.75, "lines")
    )
}

make_sorting_additive_bias_plots <- function(opts) {
  plots_dir <- opts$plots_dir
  if (is.null(plots_dir)) {
    plots_dir <- file.path(opts$project_root, "runs", opts$run_id, "plots")
  }
  dir.create(plots_dir, showWarnings = FALSE, recursive = TRUE)

  interaction <- read_result(opts, "sorting_additive_bias_detail.csv")
  save_plot(
    make_sorting_additive_crosscheck_four_panel(interaction, a_value = 0.10),
    file.path(plots_dir, "sorting_additive_crosscheck_a_pos010_four_panel.png"),
    width = 10.2, height = 3.7
  )

  save_plot(
    make_sorting_additive_crosscheck_four_panel(interaction, a_value = -0.10),
    file.path(plots_dir, "sorting_additive_crosscheck_a_neg010_four_panel.png"),
    width = 10.2, height = 3.7
  )

  invisible(plots_dir)
}

make_bias_decomposition_plots <- function(opts) {
  plots_dir <- opts$plots_dir
  if (is.null(plots_dir)) {
    plots_dir <- file.path(opts$project_root, "runs", opts$run_id, "plots")
  }
  results_dir <- opts$results_dir
  if (is.null(results_dir)) {
    results_dir <- file.path(opts$project_root, "runs", opts$run_id, "results")
  }
  dir.create(plots_dir, showWarnings = FALSE, recursive = TRUE)

  sorting_detail <- read_result(opts, "sorting_detail.csv")
  save_plot(
    make_sorting_policy_composition_plot(sorting_detail),
    file.path(plots_dir, "sorting_policy_value_treated_composition.png"),
    width = 9.4, height = 3.9
  )

  save_plot(
    make_distribution_plot(collect_additive_distribution(opts),
                           "Figure 4-6. Score Distribution Correction under Additive Bias",
                           "score value"),
    file.path(plots_dir, "additive_bias_score_distributions_with_flip_region.png"),
    width = 10.8, height = 4.45
  )

  sorting_additive_path <- file.path(results_dir, "sorting_additive_bias_summary.csv")
  if (file.exists(sorting_additive_path)) {
    make_sorting_additive_bias_plots(opts)
  }

  slope <- read_result(opts, "slope_bias_detail.csv")
  save_plot(
    make_slope_error_policy_plot(slope, "b", "Scale distortion parameter b",
                                 "Figure 4-7. Scale bias experiment: error reductions and policy value"),
    file.path(plots_dir, "slope_bias_error_reduction_vs_policy_gain.png"),
    width = 10.2, height = 4.6
  )

  invisible(plots_dir)
}

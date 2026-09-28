utils::globalVariables(c("line", "y"))

#' Estimate dual cutoffs with bootstrap confidence intervals
#'
#' Estimate rule-out and rule-in cutoffs for a binary reference group and
#' calculate bootstrap confidence intervals. Cutoffs and confidence intervals
#' are returned on the original score scale, regardless of `direction`.
#'
#' @param score Numeric vector of index-test scores.
#' @param group Numeric vector containing exactly the codes `0` (controls) and
#'   `1` (cases).
#' @param sensitivity_target Numeric target sensitivity for the rule-out
#'   cutoff.
#' @param specificity_target Numeric target specificity for the rule-in
#'   cutoff.
#' @param n_bootstrap Number of stratified bootstrap replicates. Must be at
#'   least 2.
#' @param seed Random seed used before bootstrapping.
#' @param direction Whether higher or lower scores indicate a positive test.
#'   Defaults to `"lower"`.
#' @param progress Logical; display a text progress bar when `TRUE`, or a
#'   function accepting the current iteration and total iterations. The
#'   callback form is useful for integrating progress into a Shiny app.
#'
#' @return An object of class `dcs_cutoffs`, containing `cutoffs`, `ci`,
#'   `direction`, and `n_bootstrap`.
#' @export
#' @examples
#' data(clinitest)
#' cutoffs <- get_cutoffs(
#'   score = clinitest$score,
#'   group = clinitest$groups,
#'   sensitivity_target = 0.90,
#'   specificity_target = 0.90,
#'   n_bootstrap = 20,
#'   seed = 91000,
#'   direction = "lower",
#'   progress = FALSE
#' )
#' cutoffs

get_cutoffs <- function(score, group, sensitivity_target, specificity_target,
                        n_bootstrap, seed=NULL, direction = "lower", progress = TRUE) {
  direction <- match.arg(direction, c("lower", "higher"))

  ## Internally, all calculations use lower scores as positive.
  if (direction == "higher") score <- -score

  select <- function(x, g) {
    metrics <- do.call(rbind, lapply(sort(unique(x)), function(cut) {
      positive <- x <= cut
      tp <- sum(positive & g == 1)
      fp <- sum(positive & g == 0)
      tn <- sum(!positive & g == 0)
      fn <- sum(!positive & g == 1)

      sensitivity <- tp / (tp + fn)
      specificity <- tn / (tn + fp)

      data.frame(
        cutoff = cut,
        sensitivity = sensitivity,
        specificity = specificity,
        lr_pos = sensitivity / (1 - specificity),
        lr_neg = (1 - sensitivity) / specificity
      )
    }))

    ruleout <- metrics[metrics$sensitivity >= sensitivity_target, ]
    if (nrow(ruleout)) {
      ruleout <- ruleout[order(-ruleout$specificity, ruleout$lr_neg), ]
    } else {
      ruleout <- metrics[which.min(abs(metrics$sensitivity - sensitivity_target)), ]
    }

    rulein <- metrics[metrics$specificity >= specificity_target, ]
    if (nrow(rulein)) {
      rulein <- rulein[order(-rulein$sensitivity, -rulein$lr_pos), ]
    } else {
      rulein <- metrics[which.min(abs(metrics$specificity - specificity_target)), ]
    }

    c(ruleout = ruleout$cutoff[1], rulein = rulein$cutoff[1])
  }

  stopifnot(n_bootstrap >= 2)
  if (is.logical(progress)) {
    stopifnot(length(progress) == 1, !is.na(progress))
  } else if (!is.function(progress)) {
    stop("progress must be TRUE, FALSE, or a callback function.", call. = FALSE)
  }

  analysis_cutoffs <- select(score, group)
  controls <- which(group == 0)
  cases <- which(group == 1)

  set.seed(seed)
  bootstrap <- matrix(
    NA_real_,
    nrow = 2,
    ncol = n_bootstrap,
    dimnames = list(c("ruleout", "rulein"), NULL)
  )

  if (isTRUE(progress)) {
    bootstrap_progress <- utils::txtProgressBar(
      min = 0, max = n_bootstrap, style = 3
    )
  }

  for (b in seq_len(n_bootstrap)) {
    i <- c(
      sample(controls, length(controls), replace = TRUE),
      sample(cases, length(cases), replace = TRUE)
    )
    bootstrap[, b] <- select(score[i], group[i])

    if (isTRUE(progress)) {
      utils::setTxtProgressBar(bootstrap_progress, b)
    }
    if (is.function(progress)) progress(b, n_bootstrap)
  }

  if (isTRUE(progress)) close(bootstrap_progress)

  rownames(bootstrap) <- c("ruleout", "rulein")
  analysis_ci <- t(apply(
    bootstrap, 1, stats::quantile, probs = c(.025, .975), na.rm = TRUE
  ))

  reported_cutoffs <- analysis_cutoffs
  reported_ci <- analysis_ci

  if (direction == "higher") {
    reported_cutoffs <- -analysis_cutoffs
    reported_ci[, 1] <- -analysis_ci[, 2]
    reported_ci[, 2] <- -analysis_ci[, 1]
  }

  structure(
    list(
      cutoffs = reported_cutoffs,
      ci = reported_ci,
      direction = direction,
      n_bootstrap = n_bootstrap
    ),
    class = "dcs_cutoffs"
  )
}

#' Print dual-cutoff results
#'
#' @param x A `dcs_cutoffs` object.
#' @param digits Number of decimal places to print.
#' @param ... Additional arguments, currently ignored.
#' @return `x`, invisibly.
#' @export
print.dcs_cutoffs <- function(x, digits = 2, ...) {
  if (!inherits(x, "dcs_cutoffs")) {
    stop("x must be an object of class dcs_cutoffs.")
  }

  fmt <- function(value) formatC(value, format = "f", digits = digits)

  cat("Dual-cutoff selection\n")
  cat("Direction:", x$direction, "\n")
  cat(
    "Rule-out cutoff:", fmt(x$cutoffs["ruleout"]),
    " [95% CI:", fmt(x$ci["ruleout", 1]),
    ",", fmt(x$ci["ruleout", 2]), "]\n"
  )
  cat(
    "Rule-in cutoff:", fmt(x$cutoffs["rulein"]),
    " [95% CI:", fmt(x$ci["rulein", 1]),
    ",", fmt(x$ci["rulein", 2]), "]\n"
  )

  invisible(x)
}

#' Classify observations using dual cutoffs
#'
#' @param score Numeric vector of scores to classify.
#' @param group Numeric vector containing the reference-group codes `0` and
#'   `1`.
#' @param cutoffs A `dcs_cutoffs` object returned by [get_cutoffs()].
#'
#' @return An object of class `dcs_classified`, containing the classified
#'   `data`, diagnostic `metrics`, and the supplied `cutoffs` object.
#' @export
#' @examples
#' data(clinitest)
#' cutoffs <- get_cutoffs(
#'   clinitest$score, clinitest$groups, 0.90, 0.90,
#'   n_bootstrap = 20, seed = 91000, direction = "lower", progress = FALSE
#' )
#' classified <- classify_subjects(clinitest$score, clinitest$groups, cutoffs)
#' head(classified$data)
classify_subjects <- function(score, group, cutoffs) {
  if (!inherits(cutoffs, "dcs_cutoffs")) {
    stop("cutoffs must be an object of class dcs_cutoffs.")
  }

  direction <- match.arg(cutoffs$direction, c("lower", "higher"))
  cutoff_values <- cutoffs$cutoffs
  analysis_score <- if (direction == "higher") -score else score
  analysis_cutoffs <- if (direction == "higher") -cutoff_values else cutoff_values

  negative <- analysis_score > analysis_cutoffs["ruleout"]
  positive <- analysis_score <= analysis_cutoffs["rulein"]

  region <- ifelse(
    negative & !positive, "negative",
    ifelse(positive & !negative, "positive", "gray")
  )

  status <- rep("Gray zone", length(score))
  status[region == "positive" & group == 1] <- "TP"
  status[region == "positive" & group == 0] <- "FP"
  status[region == "negative" & group == 1] <- "FN"
  status[region == "negative" & group == 0] <- "TN"

  tp <- sum(status == "TP")
  tn <- sum(status == "TN")
  fp <- sum(status == "FP")
  fn <- sum(status == "FN")
  classified <- tp + tn + fp + fn

  ratio <- function(a, b) if (b == 0) NA_real_ else a / b
  sensitivity <- ratio(tp, tp + fn)
  specificity <- ratio(tn, tn + fp)

  metrics <- c(
    negative = sum(region == "negative"),
    gray = sum(region == "gray"),
    positive = sum(region == "positive"),
    classified = classified,
    TP = tp, TN = tn, FP = fp, FN = fn,
    prevalence = ratio(tp + fn, classified),
    sensitivity = sensitivity,
    specificity = specificity,
    false_positive_rate = ratio(fp, fp + tn),
    false_negative_rate = ratio(fn, fn + tp),
    accuracy = ratio(tp + tn, classified),
    PPV = ratio(tp, tp + fp),
    NPV = ratio(tn, tn + fn),
    LR_positive = ratio(sensitivity, ratio(fp, fp + tn)),
    LR_negative = ratio(ratio(fn, fn + tp), specificity)
  )

  structure(
    list(
      data = data.frame(
        score = score,
        group = group,
        region = region,
        status = status
      ),
      metrics = metrics,
      cutoffs = cutoffs
    ),
    class = "dcs_classified"
  )
}

#' Summarise dual-cutoff classifications
#'
#' @param object A `dcs_classified` object returned by
#'   [classify_subjects()].
#' @param ... Additional arguments, currently ignored.
#' @return A data frame with `Section`, `Metric`, and `Value` columns.
#' @export
#' @examples
#' data(clinitest)
#' cutoffs <- get_cutoffs(
#'   clinitest$score, clinitest$groups, 0.90, 0.90,
#'   n_bootstrap = 20, seed = 91000, direction = "lower", progress = FALSE
#' )
#' classified <- classify_subjects(clinitest$score, clinitest$groups, cutoffs)
#' summary(classified)
summary.dcs_classified <- function(object, ...) {
  if (!inherits(object, "dcs_classified")) {
    stop("object must be an object of class dcs_classified.")
  }

  m <- object$metrics
  cobj <- object$cutoffs
  count_pct <- function(n, denominator) {
    if (denominator == 0) sprintf("%d (NA%%)", n)
    else sprintf("%d (%.1f%%)", n, 100 * n / denominator)
  }
  percent <- function(x) {
    if (!is.finite(x)) "NA%" else sprintf("%.1f%%", 100 * x)
  }
  ratio <- function(x) {
    if (is.nan(x) || is.na(x)) "NA"
    else if (is.infinite(x)) "Inf"
    else sprintf("%.2f", x)
  }

  n <- nrow(object$data)
  n_cases <- sum(object$data$group == 1)
  n_controls <- sum(object$data$group == 0)
  negative_region <- m[["negative"]]
  positive_region <- m[["positive"]]
  gray_region <- m[["gray"]]
  classified_region <- m[["classified"]]
  gray_cases <- sum(object$data$region == "gray" & object$data$group == 1)
  gray_controls <- sum(object$data$region == "gray" & object$data$group == 0)

  data.frame(
    Section = c(
      rep("Cutoffs", 6),
      rep("Regions", 6),
      rep("Status - classified only", 4),
      rep("Diagnostic indices - classified only", 10)
    ),
    Metric = c(
      "Rule-out cutoff",
      "Rule-in cutoff",
      "Bootstrap iterations",
      "Rule-out cutoff - 95% CI",
      "Rule-in cutoff - 95% CI",
      "Gray zone",
      "Rule-out region",
      "Rule-in region",
      "Gray zone",
      "Classified sample",
      "Gray zone - Cases",
      "Gray zone - Controls",
      "True Positives (TP)",
      "True Negatives (TN)",
      "False Positives (FP)",
      "False Negatives (FN)",
      "Prevalence",
      "Sensitivity",
      "Specificity",
      "False positive rate",
      "False negative rate",
      "Accuracy",
      "Positive predictive value",
      "Negative predictive value",
      "Positive likelihood ratio",
      "Negative likelihood ratio"
    ),
    Value = c(
      sprintf("%.2f", cobj$cutoffs["ruleout"]),
      sprintf("%.2f", cobj$cutoffs["rulein"]),
      as.character(cobj$n_bootstrap),
      sprintf("[%.2f, %.2f]", cobj$ci["ruleout", 1], cobj$ci["ruleout", 2]),
      sprintf("[%.2f, %.2f]", cobj$ci["rulein", 1], cobj$ci["rulein", 2]),
      sprintf("%.2f, %.2f", min(cobj$cutoffs), max(cobj$cutoffs)),
      count_pct(negative_region, n),
      count_pct(positive_region, n),
      count_pct(gray_region, n),
      count_pct(classified_region, n),
      count_pct(gray_cases, n_cases),
      count_pct(gray_controls, n_controls),
      count_pct(m[["TP"]], classified_region),
      count_pct(m[["TN"]], classified_region),
      count_pct(m[["FP"]], classified_region),
      count_pct(m[["FN"]], classified_region),
      percent(m[["prevalence"]]),
      percent(m[["sensitivity"]]),
      percent(m[["specificity"]]),
      percent(m[["false_positive_rate"]]),
      percent(m[["false_negative_rate"]]),
      percent(m[["accuracy"]]),
      percent(m[["PPV"]]),
      percent(m[["NPV"]]),
      ratio(m[["LR_positive"]]),
      ratio(m[["LR_negative"]])
    ),
    stringsAsFactors = FALSE
  )
}

#' Plot dual-cutoff classifications
#'
#' @param score Numeric vector of scores.
#' @param group Numeric vector containing the reference-group codes `0` and
#'   `1`.
#' @param status Classification status for each observation, using `TP`, `TN`,
#'   `FP`, `FN`, and `Gray zone`.
#' @param cutoffs A `dcs_cutoffs` object returned by [get_cutoffs()].
#' @param score_label Label for the y-axis.
#' @return A ggplot object.
#' @export
#' @import ggplot2
#' @examples
#' data(clinitest)
#' cutoffs <- get_cutoffs(
#'   clinitest$score, clinitest$groups, 0.90, 0.90,
#'   n_bootstrap = 20, seed = 91000, direction = "lower", progress = FALSE
#' )
#' classified <- classify_subjects(clinitest$score, clinitest$groups, cutoffs)
#' dcs_plot(
#'   clinitest$score, clinitest$groups, classified$data$status, cutoffs
#' )
dcs_plot <- function(score, group, status, cutoffs, score_label = "Score") {
  if (!inherits(cutoffs, "dcs_cutoffs")) {
    stop("cutoffs must be an object of class dcs_cutoffs.")
  }

  direction <- match.arg(cutoffs$direction, c("lower", "higher"))
  plot_cutoffs <- cutoffs$cutoffs
  plot_ci <- cutoffs$ci

  plot_data <- data.frame(
    score = score,
    group = factor(group, levels = c(0, 1), labels = c("Controls", "Cases")),
    status = factor(
      status,
      levels = c("TP", "TN", "FP", "FN", "Gray zone")
    )
  )

  lines_df <- data.frame(
    line = factor(
      c("Rule-out cutoff", "Rule-in cutoff"),
      levels = c("Rule-out cutoff", "Rule-in cutoff")
    ),
    y = c(plot_cutoffs["ruleout"], plot_cutoffs["rulein"])
  )

  ci_band_layers <- list()

  if (all(is.finite(plot_ci["ruleout", ]))) {
    ci_band_layers <- c(
      ci_band_layers,
      list(
        annotate(
          "rect",
          xmin = -Inf,
          xmax = Inf,
          ymin = plot_ci["ruleout", 1],
          ymax = plot_ci["ruleout", 2],
          fill = "#2E7D32",
          alpha = 0.14
        )
      )
    )
  }

  if (all(is.finite(plot_ci["rulein", ]))) {
    ci_band_layers <- c(
      ci_band_layers,
      list(
        annotate(
          "rect",
          xmin = -Inf,
          xmax = Inf,
          ymin = plot_ci["rulein", 1],
          ymax = plot_ci["rulein", 2],
          fill = "#C62828",
          alpha = 0.14
        )
      )
    )
  }

  ggplot(plot_data, aes(x = group, y = score)) +
    annotate(
      "rect",
      xmin = -Inf,
      xmax = Inf,
      ymin = min(plot_cutoffs),
      ymax = max(plot_cutoffs),
      fill = "#BDBDBD",
      alpha = 0.22
    ) +
    ci_band_layers +
    geom_violin(
      aes(fill = group),
      alpha = 0.30,
      width = 0.9,
      trim = FALSE,
      color = "#4D4D4D",
      linewidth = 0.25
    ) +
    geom_jitter(
      aes(color = status),
      width = 0.12,
      height = 0,
      size = 2.3,
      alpha = 0.90
    ) +
    geom_hline(
      data = lines_df,
      aes(yintercept = y, linetype = line),
      color = "#000000",
      alpha = 0,
      show.legend = TRUE
    ) +
    geom_hline(
      yintercept = plot_cutoffs["ruleout"],
      color = "#2E7D32",
      linetype = "dashed",
      linewidth = 1.15
    ) +
    geom_hline(
      yintercept = plot_cutoffs["rulein"],
      color = "#C62828",
      linetype = "dashed",
      linewidth = 1.15
    ) +
    scale_fill_manual(
      values = c("Controls" = "#BFE6FF", "Cases" = "#E8A0A0")
    ) +
    scale_color_manual(
      values = c(
        "TP" = "#00897B",
        "TN" = "#2E7D32",
        "FP" = "#F57C00",
        "FN" = "#C62828",
        "Gray zone" = "#9E9E9E"
      )
    ) +
    scale_linetype_manual(
      values = c(
        "Rule-out cutoff" = "dashed",
        "Rule-in cutoff" = "dashed"
      )
    ) +
    labs(
      x = NULL,
      y = score_label,
      fill = NULL,
      color = "Classification status",
      linetype = "Cut-off lines"
    ) +
    theme_classic(base_size = 14) +
    guides(
      fill = "none",
      linetype = guide_legend(
        order = 2,
        override.aes = list(
          color = c("#2E7D32", "#C62828"),
          alpha = 1,
          linewidth = 1.15
        )
      ),
      color = guide_legend(order = 1)
    ) +
    theme(
      legend.position = "right",
      panel.grid.major.y = element_line(
        color = "#E6E6E6",
        linewidth = 0.25
      ),
      axis.text = element_text(color = "#000000"),
      axis.title.y = element_text(color = "#000000"),
      legend.title = element_text(face = "bold")
    )
}

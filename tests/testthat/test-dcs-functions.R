test_that("get_cutoffs returns a dcs_cutoffs object", {
  score <- c(1, 2, 3, 4, 5, 6)
  group <- c(0, 0, 0, 1, 1, 1)

  result <- get_cutoffs(
    score = score,
    group = group,
    sensitivity_target = 0.90,
    specificity_target = 0.90,
    n_bootstrap = 20,
    seed = 42,
    direction = "lower",
    progress = FALSE
  )

  expect_s3_class(result, "dcs_cutoffs")
  expect_named(result, c("cutoffs", "ci", "direction", "n_bootstrap"))
  expect_named(result$cutoffs, c("ruleout", "rulein"))
  expect_equal(dim(result$ci), c(2L, 2L))
  expect_equal(result$direction, "lower")
  expect_equal(result$n_bootstrap, 20)
  expect_true(result$cutoffs[["ruleout"]] >= result$cutoffs[["rulein"]])
})

test_that("clinitest contains an intuitive 0 to 100 score", {
  data(clinitest)

  expect_true(all(clinitest$score >= 0 & clinitest$score <= 100))
  expect_true(all(clinitest$score == round(clinitest$score, 1)))
  expect_equal(names(clinitest), c("score", "groups"))
  expect_lt(mean(clinitest$score[clinitest$groups == 1]), mean(clinitest$score[clinitest$groups == 0]))

  cutoffs <- get_cutoffs(
    clinitest$score,
    clinitest$groups,
    sensitivity_target = 0.90,
    specificity_target = 0.90,
    n_bootstrap = 20,
    seed = 91000,
    direction = "lower",
    progress = FALSE
  )
  classified <- classify_subjects(clinitest$score, clinitest$groups, cutoffs)
  expect_gt(classified$metrics[["accuracy"]], 0.50)
})

test_that("subjects can be classified and summarised", {
  cutoffs <- structure(
    list(
      cutoffs = c(ruleout = 5, rulein = 3),
      ci = rbind(ruleout = c(4, 6), rulein = c(2, 4)),
      direction = "lower",
      n_bootstrap = 20
    ),
    class = "dcs_cutoffs"
  )
  score <- c(1, 2, 3, 4, 5, 6)
  group <- c(0, 0, 0, 1, 1, 1)

  result <- classify_subjects(score, group, cutoffs)

  expect_s3_class(result, "dcs_classified")
  expect_equal(result$data$status, c("FP", "FP", "FP", "Gray zone", "Gray zone", "FN"))
  expect_equal(unname(result$metrics[c("TP", "TN", "FP", "FN")]), c(0, 0, 3, 1))
  expect_equal(nrow(summary(result)), 26L)
  expect_output(print(cutoffs), "Dual-cutoff selection")
})

test_that("dcs_plot returns a ggplot object", {
  cutoffs <- structure(
    list(
      cutoffs = c(ruleout = 5, rulein = 3),
      ci = rbind(ruleout = c(4, 6), rulein = c(2, 4)),
      direction = "lower",
      n_bootstrap = 20
    ),
    class = "dcs_cutoffs"
  )
  score <- c(1, 2, 3, 4, 5, 6)
  group <- c(0, 0, 0, 1, 1, 1)
  status <- c("FP", "FP", "FP", "Gray zone", "Gray zone", "FN")

  expect_s3_class(dcs_plot(score, group, status, cutoffs), "ggplot")
})

# Generate the example dataset shipped with RclinimetriX.
set.seed(20260913)

clinitest <- data.frame(
  score = round(pmin(pmax(c(
    rnorm(50, mean = 60, sd = 12),
    rnorm(50, mean = 40, sd = 12)
  ), 0), 100), 1),
  group = rep(c(0, 1), each = 50)
)

usethis::use_data(clinitest, overwrite = TRUE)

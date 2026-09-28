#' Simulated clinimetric test data
#'
#' A reproducible simulated dataset for demonstrating the RclinimetriX DCS
#' functions. The `groups` variable uses `0` for controls and `1` for cases;
#' cases have a higher average score than controls.
#'
#' @format A data frame with 100 rows and 2 variables:
#' \describe{
#'   \item{score}{A simulated score with group-specific normal distributions, bounded between 0 and 100, and rounded to one decimal place.}
#'   \item{groups}{A binary reference-group indicator: `0` for controls and `1` for cases.}
#' }
#' @source Simulated with `set.seed(20260913)` and `rnorm()`.
#' @examples
#' data(clinitest)
#' head(clinitest)
"clinitest"

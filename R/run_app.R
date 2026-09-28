#' Run the RclinimetriX Shiny application
#'
#' Launch the Shiny application bundled with the installed package. The app
#' uses the package's exported DCS functions, so it always runs the installed
#' package implementation.
#'
#' @param ... Arguments passed to [shiny::runApp()].
#' @return The result of [shiny::runApp()]. This function normally blocks while
#'   the application is running.
#' @export
run_app <- function(...) {
  required <- c("shiny", "bslib", "readxl", "DT", "writexl")
  missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) {
    stop(
      "The Shiny app requires these packages: ",
      paste(missing, collapse = ", "),
      ". Install them before calling run_app().",
      call. = FALSE
    )
  }

  app_dir <- system.file("shiny-examples", "RclinimetriX", package = "RclinimetriX")
  if (!nzchar(app_dir) || !dir.exists(app_dir)) {
    stop("The bundled Shiny app was not found in the installed package.", call. = FALSE)
  }

  shiny::runApp(appDir = app_dir, ...)
}

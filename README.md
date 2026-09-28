# RclinimetriX

RclinimetriX provides clinimetric tools for diagnostic accuracy analyses.
The package currently includes the Dual Cutoff Selection (DCS) analysis.

## Installation


```r
install.packages("remotes")

remotes::install_github(
  "Cirorilardi/RclinimetriX",
  dependencies = TRUE
)
```

The `dependencies = TRUE` option installs the package dependencies, including
the packages required by the Shiny application.

## Run the Shiny application

After installation, start the bundled application with:

```r
RclinimetriX::run_app()
```

The app opens locally in a browser. To choose a specific port or prevent the
browser from opening automatically, pass arguments through to `shiny::runApp()`:

```r
RclinimetriX::run_app(
  port = 8080,
  launch.browser = FALSE
)
```

Replace `8080` with another available numeric port if needed.

## Input data

The DCS module accepts a CSV or Excel file (.csv, .xlsx, or .xls) containing:

- a group variable coded `0` for controls and `1` for cases;
- a numeric score variable.

Select both variables in the sidebar, choose whether higher or lower scores
indicate a positive test, set the diagnostic targets, and press **Run**.

The output includes rule-out and rule-in cutoffs, bootstrap confidence
intervals, a classification summary, and a diagnostic plot.

## Package functions

The main exported functions are:

```r
cutoffs <- RclinimetriX::get_cutoffs(
  score = score,
  group = group,
  sensitivity_target = 0.90,
  specificity_target = 0.90,
  n_bootstrap = 1000,
  seed = 91000,
  direction = "lower"
)

classified <- RclinimetriX::classify_subjects(score, group, cutoffs)
summary(classified)
RclinimetriX::dcs_plot(
  score,
  group,
  classified$data$status,
  cutoffs
)
```

The Shiny application calls these same package functions directly, so updates
to the package implementation are reflected in the application.


# Citation

If you use RclinimetriX and/or its scripts, please cite the repository and the associated manuscript:

Anonymous Author et al. (in submission). When two is better than one: An R tutorial for the Dual Cut-off Selection (DCS) strategy. Journal of Neuropsychology.

The final citation and DOI will be added after publication.

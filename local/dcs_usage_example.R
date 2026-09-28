#### Example use of the dual-cutoff functions
#### Run this file with Rscripts/ as the working directory.
####

library(RclinimetriX)
library(writexl)

## Example data ##

set.seed(1)
Group <- rep(c(1, 0), each = 50)
Score <- rnorm(100) + Group

raw <- data.frame(Group, Score)
#### To use Excel data, replace the line above with:
#### raw <- readxl::read_excel("input.xlsx")
####

## Settings ##

group_var <- "Group"                 ## 0 = controls, 1 = cases ##
score_var <- "Score"
direction <- "higher"                ## or "lower" ##
target_sensitivity <- .90
target_specificity <- .90
n_bootstrap <- 1000
bootstrap_seed <- 91000
progress <- TRUE  ## pregress bar for bootstrap computation
output_file <- "Output_table.xlsx"

## Prepare the data ##

dat <- data.frame(
  group = as.numeric(raw[[group_var]]),
  score = as.numeric(raw[[score_var]])
)
dat <- na.omit(dat)
stopifnot(all(dat$group %in% c(0, 1)), length(unique(dat$group)) == 2)

## Estimate cutoffs and confidence intervals ##
#RclinimetriX::get_cutoffs()

cutoffs <- get_cutoffs(
  score = dat$score,
  group = dat$group,
  sensitivity_target = target_sensitivity,
  specificity_target = target_specificity,
  n_bootstrap = n_bootstrap,
  seed = bootstrap_seed,
  direction = direction,
  progress = progress
)
print(cutoffs)

## Classify subjects ##
#RclinimetriX::classify_subjects()

classified <- classify_subjects(
  score = dat$score,
  group = dat$group,
  cutoffs = cutoffs
)

## Print and export a summary in output_table format ##

output_table <- summary(classified)
print(output_table, row.names = FALSE)
write_xlsx(output_table, output_file)

## Plot ##
##RclinimetriX::dcs_plot()

print(dcs_plot(
  score = dat$score,
  group = dat$group,
  status = classified$data$status,
  cutoffs = cutoffs,
  score_label = score_var
))

### from the help

data(clinitest)
cutoffs <- get_cutoffs(
  clinitest$score, clinitest$groups, 0.90, 0.90,
  n_bootstrap = 1000, seed = 91000, progress = TRUE
)
classified <- classify_subjects(clinitest$score, clinitest$groups, cutoffs)
head(classified$data)
summary(classified)
summary(clinitest$score)

RclinimetriX::run_app()

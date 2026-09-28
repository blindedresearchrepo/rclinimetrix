# Dual Cut-off Selection (DCS) Strategy

# Run this script with `Rscripts/` as the working directory.

source("dcs_functions_neurotutorials.R")
library(readxl)
library(writexl)

# Import Excel file

file_path <- file.choose()
raw <- as.data.frame(read_excel(file_path))

# To use simulated data, replace the preceding two lines with:
# set.seed(3)
# Group <- rep(c(1, 0), each = 50)
# Score <- rnorm(100) - Group
# raw <- data.frame(Group, Score)

# Settings

group_var <- "Group" # 0 = controls; 1 = cases
score_var <- "Score"
direction <- "lower" # "lower" or "higher"
target_sensitivity <- .90
target_specificity <- .90
n_bootstrap <- 1000
bootstrap_seed <- NULL # Set to NULL to generate a new bootstrap resampling sequence at each run.
progress <- TRUE # Display a progress bar during bootstrap computation.
output_file <- "Output_table.xlsx"

# Prepare data

dat <- data.frame(
  group = as.numeric(raw[[group_var]]),
  score = as.numeric(raw[[score_var]])
)
dat <- na.omit(dat)
stopifnot(all(dat$group %in% c(0, 1)), length(unique(dat$group)) == 2)

# Estimate cutoffs and confidence intervals

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

# Classify subjects

classified <- classify_subjects(
  score = dat$score,
  group = dat$group,
  cutoffs = cutoffs
)

# Print and export the summary as an output table

output_table <- summary(classified)
print(output_table, row.names = FALSE)
write_xlsx(output_table, output_file)

# Plot results

print(dcs_plot(
  score = dat$score,
  group = dat$group,
  status = classified$data$status,
  cutoffs = cutoffs,
  score_label = score_var
))

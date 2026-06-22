#!/usr/bin/env Rscript

# calc_practice_attainment.R
# Compute practice-level QOF attainment proportions by domain and scenario.
#
# Expects an input file (CSV or RDS) containing rows with the following columns:
# - practice_id (or practice_code or practice)
# - domain
# - scenario
# - numerator
# - denominator
#
# The script will:
# - aggregate rows by practice_id + domain + scenario (summing numerators & denominators)
# - compute proportion = numerator / denominator (NA when denominator == 0)
# - clip proportions to [0,1]
# - write a detailed per-practice file and a per-domain+scenario summary file
#
# Usage:
# Rscript scripts/calc_practice_attainment.R -i data/qof_counts.csv -o output/practice_attainment.csv -s output/attainment_summary.csv

suppressPackageStartupMessages({
  has_optparse <- requireNamespace("optparse", quietly = TRUE)
  if (!has_optparse) stop("Please install the 'optparse' package: install.packages('optparse')")
  has_dplyr <- requireNamespace("dplyr", quietly = TRUE)
  if (!has_dplyr) stop("Please install the 'dplyr' package: install.packages('dplyr')")
  library(optparse)
  library(dplyr)
})

option_list <- list(
  make_option(c("-i", "--input"), type = "character", help = "Input CSV or RDS file with required columns"),
  make_option(c("-o", "--out"), type = "character", default = "practice_attainment_by_practice.csv",
              help = "Output CSV path for per-practice attainment [default: %default]"),
  make_option(c("-s", "--summary"), type = "character", default = "practice_attainment_summary.csv",
              help = "Output CSV path for domain+scenario summary [default: %default]")
)
opt_parser <- OptionParser(option_list = option_list)
opt <- parse_args(opt_parser)

if (is.null(opt$input)) {
  print_help(opt_parser)
  stop("Please provide --input")
}

# Read input (CSV or RDS)
read_input <- function(path) {
  if (grepl("\\.rds$", path, ignore.case = TRUE)) {
    readRDS(path)
  } else {
    read.csv(path, stringsAsFactors = FALSE)
  }
}

df <- read_input(opt$input)

# Detect practice id column (accept common aliases)
practice_cols <- c("practice_id", "practiceCode", "practice_code", "practice", "practiceCode")
found_practice <- intersect(tolower(names(df)), tolower(practice_cols))
if (length(found_practice) == 0) {
  stop("No practice identifier column found. Expected one of: practice_id, practice_code, practice")
}
# pick the first matching original name
practice_col <- names(df)[match(found_practice[1], tolower(names(df)))]
# Standardise column names we will use
names(df)[names(df) == practice_col] <- "practice_id"

required_cols <- c("practice_id", "domain", "scenario", "numerator", "denominator")
miss <- setdiff(required_cols, names(df))
if (length(miss) > 0) stop(paste("Input is missing required columns:", paste(miss, collapse = ", ")))

# Ensure numeric
df$numerator <- as.numeric(df$numerator)
df$denominator <- as.numeric(df$denominator)

# Aggregate by practice_id + domain + scenario
by_practice <- df %>%
  group_by(practice_id, domain, scenario) %>%
  summarise(
    numerator = sum(numerator, na.rm = TRUE),
    denominator = sum(denominator, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    proportion = ifelse(denominator > 0, numerator / denominator, NA_real_),
    proportion = pmin(pmax(proportion, 0), 1)
  )

# Create domain+scenario summary: unweighted mean, median, weighted (by denominator) mean
summary_stats <- by_practice %>%
  group_by(domain, scenario) %>%
  summarise(
    n_practices = n(),
    mean_attainment = mean(proportion, na.rm = TRUE),
    median_attainment = median(proportion, na.rm = TRUE),
    weighted_attainment = ifelse(sum(denominator, na.rm = TRUE) > 0,
                                 sum(numerator, na.rm = TRUE) / sum(denominator, na.rm = TRUE),
                                 NA_real_),
    total_numerator = sum(numerator, na.rm = TRUE),
    total_denominator = sum(denominator, na.rm = TRUE),
    .groups = "drop"
  )

# Write outputs
out_dir <- dirname(opt$out)
if (out_dir != "" && !dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
summary_dir <- dirname(opt$summary)
if (summary_dir != "" && !dir.exists(summary_dir)) dir.create(summary_dir, recursive = TRUE)

write.csv(by_practice, opt$out, row.names = FALSE)
write.csv(summary_stats, opt$summary, row.names = FALSE)

cat(sprintf("Wrote per-practice attainment to '%s' (%d rows)\n", opt$out, nrow(by_practice)))
cat(sprintf("Wrote domain+scenario summary to '%s' (%d rows)\n", opt$summary, nrow(summary_stats)))

# Also print a small head of the outputs for quick debug when running interactively
cat("\nSample (per-practice)\n")
print(utils::head(by_practice, 10))
cat("\nSample (summary)\n")
print(utils::head(summary_stats, 20))

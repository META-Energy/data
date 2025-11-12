# Setup ----

# Load packages
library(haven)
library(dplyr)
library(plotly)

# Load required functions
source("~/GitHub/MORPEP/META_CMP/data/analysis/R/meta_analysis.R")
source("~/GitHub/MORPEP/META_CMP/data/analysis/R/apply_winsorization.R")
source("~/GitHub/MORPEP/META_CMP/data/analysis/R/kasy_MetaStudiesFunctions.R")
source("~/GitHub/MORPEP/META_CMP/data/analysis/R/kasy_RobustVariance.R")
source("~/GitHub/MORPEP/META_CMP/data/analysis/R/kasy_MetaStudiesPlots.R")
source("~/GitHub/MORPEP/META_CMP/data/analysis/R/create_z_histogram.R")

# Define helper function to calculate factors of change between estimates at thresholds ----
calculate_factor_changes <- function(results_df) {
  # Extract threshold rows (excluding μ, τ, df rows)
  threshold_rows <- results_df[!results_df$term %in% c("μ", "τ", "df"), ]

  # Extract the highest threshold value from the term column
  threshold_terms <- threshold_rows$term
  # Find the last threshold that contains a number
  last_threshold_idx <- max(which(grepl("[0-9]", threshold_terms)))
  last_threshold <- threshold_terms[last_threshold_idx]

  # Extract the upper bound
  # Split by comma and take the second part, then extract the number
  upper_bound <- as.numeric(gsub(
    "[^0-9.-]",
    "",
    strsplit(last_threshold, ",")[[1]][2]
  ))

  # Add implicit "1" for the highest threshold with automatic naming
  threshold_rows <- rbind(
    threshold_rows,
    data.frame(
      term = sprintf("(%.2f, ∞]", upper_bound),
      estimate = 1,
      std.error = NA,
      conf.low = NA,
      conf.high = NA,
      statistic = NA,
      p.value = NA
    )
  )

  # Create empty matrix for factor changes
  n <- nrow(threshold_rows)
  factor_matrix <- matrix(NA, nrow = n, ncol = n)

  # Calculate factor changes for all combinations
  for (i in 1:n) {
    for (j in 1:n) {
      if (i != j) {
        factor_matrix[i, j] <- threshold_rows$estimate[j] /
          threshold_rows$estimate[i]
      }
    }
  }

  # Create nice labels for rows and columns
  labels <- threshold_rows$term

  # Convert to data frame with proper labels
  factor_df <- as.data.frame(factor_matrix)
  colnames(factor_df) <- labels
  rownames(factor_df) <- labels

  # Format the numbers (round to 3 decimal places)
  factor_df[] <- lapply(factor_df, function(x) round(x, 3))

  # Replace diagonal NA values with "-"
  factor_df[is.na(factor_df)] <- "-"

  return(factor_df)
}

## Load data ----
d_path <- "data/metaenergy_final.dta"
d <- haven::read_dta(d_path)

# Add placeholders required variables or create variable names to make compatible with META CMP meta_analysis() function.
# (for the final version of this script, we could clean this up)
d$outcome <- "effect"
d$period.month <- 1
d$mean.effect <- d$coeffc
d$SE.avg <- d$sec
d$precision.avg <- 1 / d$SE.avg
d$key <- d$Key
prd <- 1
funnel_se_option <- "avg"

## Path for figures ----
figure_path <- "~/GitHub/MORPEP/META_Energy/data/analyses/replication_main_paper/R/working_paper_1/figures/"


## Drop estimates for which we only have assumed precision measures based on ----
## "significance stars".
d <- d %>%
  filter(
    !(is.na(se_orig) &
      is.na(t_orig) &
      p_val_calc %in% c(0.001, 0.01, 0.05, 0.1, 0.5))
  )

## Trim extreme values ----
trim_lev <- 0.02 # Set trimming level
d <- d %>%
  filter(between(
    sec,
    quantile(sec, trim_lev, na.rm = TRUE),
    quantile(sec, 1 - trim_lev, na.rm = TRUE)
  )) %>%
  filter(between(
    coeffc,
    quantile(coeffc, trim_lev, na.rm = TRUE),
    quantile(coeffc, 1 - trim_lev, na.rm = TRUE)
  ))
# Alternative with winsorization: Set trim_lev <- 0 and set wins_para <- 0.02 below

# Estimation for 1.96 (95 %) level ----

## Define additional function to add confidence bands to plots ----
add_confidence_bands <- function(
  current_plot,
  tidy_data,
  start_row = 4,
  band_alpha = 0.2,
  band_color = "blue"
) {
  # Define intervals based on cutoff value (assumes AK_symmetric = FALSE)
  intervals <- list(
    c(-Inf, -1.96),
    c(-1.96, 0),
    c(0, 1.96),
    c(1.96, Inf)
  )

  # Add confidence bands as rectangles
  for (i in 1:length(intervals)) {
    row_idx <- start_row + (i - 1)
    current_plot <- current_plot +
      annotate(
        "rect",
        xmin = intervals[[i]][1],
        xmax = intervals[[i]][2],
        ymin = tidy_data$conf.low[row_idx],
        ymax = tidy_data$conf.high[row_idx],
        alpha = band_alpha,
        fill = band_color
      )
  }

  # Apply styling
  current_plot <- current_plot +
    theme_minimal() +
    labs(y = "Publication probability (log scale)")

  return(current_plot)
}

## Specifiy input arguments for estimation with meta_analysis() function ----
wins_para <- 0 # Data is already trimmed
ap <- FALSE # Adequately powered (80% or more)
prec_weighted <- FALSE
cluster_se <- TRUE
AK_critvals <- c(
  # Cutoff for statistical significance
  1.96 # 95%
)
AK_symmetric <- FALSE
AK_modelmu <- "t"
AK_conf_level <- 0.89

## For all ----
ak_results <- meta_analysis(
  data = d,
  outvar = "effect",
  se_option = funnel_se_option,
  periods = prd,
  wins = wins_para,
  ap = ap,
  prec_weighted = prec_weighted,
  estimation = "AK",
  cluster_se = cluster_se,
  cutoff_val = AK_critvals,
  AK_symmetric = AK_symmetric,
  AK_modelmu = AK_modelmu,
  AK_conf_level = AK_conf_level,
  ak_plot = "pub_prob_only",
  AK_plot_prob_y_range = c(1, 60),
  ak_prob_plot_log_scale = TRUE
)

## For short run ----
ak_results_sr <- meta_analysis(
  data = d %>%
    filter(horizond == 0),
  outvar = "effect",
  se_option = funnel_se_option,
  periods = prd,
  wins = wins_para,
  ap = ap,
  prec_weighted = prec_weighted,
  estimation = "AK",
  cluster_se = cluster_se,
  cutoff_val = AK_critvals,
  AK_symmetric = AK_symmetric,
  AK_modelmu = AK_modelmu,
  AK_conf_level = AK_conf_level,
  ak_plot = "pub_prob_only",
  AK_plot_prob_y_range = c(1, 60),
  ak_prob_plot_log_scale = TRUE
)

## For long run ----
ak_results_lr <- meta_analysis(
  data = d %>%
    filter(horizond == 1),
  outvar = "effect",
  se_option = funnel_se_option,
  periods = prd,
  wins = wins_para,
  ap = ap,
  prec_weighted = prec_weighted,
  estimation = "AK",
  cluster_se = cluster_se,
  cutoff_val = AK_critvals,
  AK_symmetric = AK_symmetric,
  AK_modelmu = AK_modelmu,
  AK_conf_level = AK_conf_level,
  ak_plot = "pub_prob_only",
  AK_plot_prob_y_range = c(1, 60),
  ak_prob_plot_log_scale = TRUE
)

## Results ----
### Estimation table ----
modelsummary::modelsummary(
  list(
    "All horizons" = ak_results[[1]],
    "Short run" = ak_results_sr[[1]],
    "Long run" = ak_results_lr[[1]]
  ),
  output = "gt",
  stars = T,
  statistic = "se: {std.error}",
  conf_level = 0.89,
  title = paste0("AK estimation; cutoff: ", AK_critvals),
  gof_map = NULL
)
#### Save as PNG
modelsummary::modelsummary(
  list(
    "All horizons" = ak_results[[1]],
    "Short run" = ak_results_sr[[1]],
    "Long run" = ak_results_lr[[1]]
  ),
  output = paste0(
    figure_path,
    "publication_probability/AK estimation_cutoff_95.png"
  ),
  stars = T,
  statistic = "se: {std.error}",
  conf_level = 0.89,
  title = paste0("AK estimation; cutoff: ", AK_critvals),
  gof_map = NULL
)

### Calculate factors ----
#### Short run estimation ----
factor_table_sr <- calculate_factor_changes(ak_results_sr[[1]]$tidy)
print(factor_table_sr)
print(
  xtable::xtable(
    factor_table_sr,
    caption = "Factor Changes Between Thresholds"
  ),
  type = "latex"
)
#### Long run estimation ----
factor_table_lr <- calculate_factor_changes(ak_results_lr[[1]]$tidy)
print(factor_table_lr)
print(
  xtable::xtable(
    factor_table_lr,
    caption = "Factor Changes Between Thresholds"
  ),
  type = "latex"
)

### Publication probability plots ----
width <- 5
height <- 3
# All horizons
all_95_plot <- add_confidence_bands(ak_results[[1]]$plot, ak_results[[1]]$tidy)
all_95_plot
ggsave(
  paste0(
    figure_path,
    "publication_probability/figure_publication_probability_all_95_log.pdf"
  ),
  plot = all_95_plot,
  device = "pdf",
  width = width,
  height = height
)
# Short run
sr_95_plot <- add_confidence_bands(
  ak_results_sr[[1]]$plot,
  ak_results_sr[[1]]$tidy
)
sr_95_plot
ggsave(
  paste0(
    figure_path,
    "publication_probability/figure_publication_probability_short_run_95_log.pdf"
  ),
  plot = sr_95_plot,
  device = "pdf",
  width = width,
  height = height
)
# Long run
lr_95_plot <- add_confidence_bands(
  ak_results_lr[[1]]$plot,
  ak_results_lr[[1]]$tidy
)
lr_95_plot
ggsave(
  paste0(
    figure_path,
    "publication_probability/figure_publication_probability_long_run_95_log.pdf"
  ),
  plot = lr_95_plot,
  device = "pdf",
  width = width,
  height = height
)

# Estimation for 1.645 (90 %), 1.96 (95 %), 2.576 (99 %) levels (for appendix) ----

## Define function to add confidence bands for multiple cutoffs ----
add_confidence_bands_multiple <- function(
  current_plot,
  tidy_data,
  cutoffs,
  start_row = 4,
  band_alpha = 0.2,
  band_color = "blue"
) {
  # Create intervals based on cutoffs
  intervals <- list()
  intervals[[1]] <- c(-Inf, -cutoffs[3]) # Below -2.576
  intervals[[2]] <- c(-cutoffs[3], -cutoffs[2]) # -2.576 to -1.96
  intervals[[3]] <- c(-cutoffs[2], -cutoffs[1]) # -1.96 to -1.645
  intervals[[4]] <- c(-cutoffs[1], 0) # -1.645 to 0
  intervals[[5]] <- c(0, cutoffs[1]) # 0 to 1.645
  intervals[[6]] <- c(cutoffs[1], cutoffs[2]) # 1.645 to 1.96
  intervals[[7]] <- c(cutoffs[2], cutoffs[3]) # 1.96 to 2.576
  intervals[[8]] <- c(cutoffs[3], Inf) # Above 2.576

  # Add confidence bands as rectangles
  for (i in 1:length(intervals)) {
    row_idx <- start_row + (i - 1)
    current_plot <- current_plot +
      annotate(
        "rect",
        xmin = intervals[[i]][1],
        xmax = intervals[[i]][2],
        ymin = tidy_data$conf.low[row_idx],
        ymax = tidy_data$conf.high[row_idx],
        alpha = band_alpha,
        fill = band_color
      )
  }

  # Apply styling
  current_plot <- current_plot +
    theme_minimal() +
    labs(y = "Publication probability (log scale)")

  return(current_plot)
}

## Specifiy input arguments for estimation with meta_analysis() function ----
AK_critvals_all <- c(
  # Cutoffs for statistical significance
  1.645, # 90%
  1.96, # 95%
  2.576 # 99%
)

## For all ----
ak_results_all_cutoffs <- meta_analysis(
  data = d,
  outvar = "effect",
  se_option = funnel_se_option,
  periods = prd,
  wins = wins_para,
  ap = ap,
  prec_weighted = prec_weighted,
  estimation = "AK",
  cluster_se = cluster_se,
  cutoff_val = AK_critvals_all,
  AK_symmetric = AK_symmetric,
  AK_modelmu = AK_modelmu,
  AK_conf_level = AK_conf_level,
  ak_plot = "pub_prob_only",
  AK_plot_prob_y_range = c(1, 70),
  ak_prob_plot_log_scale = TRUE
)

## For short run ----
ak_results_sr_all_cutoffs <- meta_analysis(
  data = d %>%
    filter(horizond == 0),
  outvar = "effect",
  se_option = funnel_se_option,
  periods = prd,
  wins = wins_para,
  ap = ap,
  prec_weighted = prec_weighted,
  estimation = "AK",
  cluster_se = cluster_se,
  cutoff_val = AK_critvals_all,
  AK_symmetric = AK_symmetric,
  AK_modelmu = AK_modelmu,
  AK_conf_level = AK_conf_level,
  ak_plot = "pub_prob_only",
  AK_plot_prob_y_range = c(1, 70),
  ak_prob_plot_log_scale = TRUE
)

## For long run ----
ak_results_lr_all_cutoffs <- meta_analysis(
  data = d %>%
    filter(horizond == 1),
  outvar = "effect",
  se_option = funnel_se_option,
  periods = prd,
  wins = wins_para,
  ap = ap,
  prec_weighted = prec_weighted,
  estimation = "AK",
  cluster_se = cluster_se,
  cutoff_val = AK_critvals_all,
  AK_symmetric = AK_symmetric,
  AK_modelmu = AK_modelmu,
  AK_conf_level = AK_conf_level,
  ak_plot = "pub_prob_only",
  AK_plot_prob_y_range = c(1, 70),
  ak_prob_plot_log_scale = TRUE
)

## Results ----
### Estimation table ----
modelsummary::modelsummary(
  list(
    "All horizons" = ak_results_all_cutoffs[[1]],
    "Short run" = ak_results_sr_all_cutoffs[[1]],
    "Long run" = ak_results_lr_all_cutoffs[[1]]
  ),
  output = "gt",
  stars = T,
  statistic = "se: {std.error}",
  conf_level = 0.89,
  title = paste0(
    "AK estimation; cutoffs: ",
    AK_critvals_all[1],
    ", ",
    AK_critvals_all[2],
    ", ",
    AK_critvals_all[3]
  ),
  gof_map = NULL
)
#### Save as PNG
modelsummary::modelsummary(
  list(
    "All horizons" = ak_results_all_cutoffs[[1]],
    "Short run" = ak_results_sr_all_cutoffs[[1]],
    "Long run" = ak_results_lr_all_cutoffs[[1]]
  ),
  output = paste0(
    figure_path,
    "publication_probability/AK estimation_cutoffs_90_95_99.png"
  ),
  stars = T,
  statistic = "se: {std.error}",
  conf_level = 0.89,
  title = paste0(
    "AK estimation; cutoffs: ",
    AK_critvals_all[1],
    ", ",
    AK_critvals_all[2],
    ", ",
    AK_critvals_all[3]
  ),
  gof_map = NULL
)

### Calculat factors ----
#### Short run estimation ----
factor_table_sr_all_cutoffs <- calculate_factor_changes(
  ak_results_sr_all_cutoffs[[1]]$tidy
)
print(factor_table_sr_all_cutoffs)
print(
  xtable::xtable(
    factor_table_sr_all_cutoffs,
    caption = "Factor Changes Between Thresholds"
  ),
  type = "latex"
)
#### Long run estimation ----
factor_table_lr_all_cutoffs <- calculate_factor_changes(
  ak_results_lr_all_cutoffs[[1]]$tidy
)
print(factor_table_lr_all_cutoffs)
print(
  xtable::xtable(
    factor_table_lr_all_cutoffs,
    caption = "Factor Changes Between Thresholds"
  ),
  type = "latex"
)

### Publication probability plots ----
width <- 5
height <- 3
# All horizons
all_multiple_plot <- add_confidence_bands_multiple(
  ak_results_all_cutoffs[[1]]$plot,
  ak_results_all_cutoffs[[1]]$tidy,
  AK_critvals_all
)
all_multiple_plot
ggsave(
  paste0(
    figure_path,
    "publication_probability/figure_publication_probability_all_all_cutoffs_log.pdf"
  ),
  plot = all_multiple_plot,
  device = "pdf",
  width = width,
  height = height
)

# Short run
sr_multiple_plot <- add_confidence_bands_multiple(
  ak_results_sr_all_cutoffs[[1]]$plot,
  ak_results_sr_all_cutoffs[[1]]$tidy,
  AK_critvals_all
)
sr_multiple_plot <- sr_multiple_plot +
  # Adjust y-axis limits to show all CIs
  ggplot2::scale_y_log10(limits = c(0.4, 60))
sr_multiple_plot
ggsave(
  paste0(
    figure_path,
    "publication_probability/figure_publication_probability_short_run_all_cutoffs_log.pdf"
  ),
  plot = sr_multiple_plot,
  device = "pdf",
  width = width,
  height = height
)

# Long run
lr_multiple_plot <- add_confidence_bands_multiple(
  ak_results_lr_all_cutoffs[[1]]$plot,
  ak_results_lr_all_cutoffs[[1]]$tidy,
  AK_critvals_all
)
lr_multiple_plot <- lr_multiple_plot +
  # Adjust y-axis limits to show all CIs
  ggplot2::scale_y_log10(limits = c(0.7, 40))
lr_multiple_plot
ggsave(
  paste0(
    figure_path,
    "publication_probability/figure_publication_probability_long_run_all_cutoffs_log.pdf"
  ),
  plot = lr_multiple_plot,
  device = "pdf",
  width = width,
  height = height
)

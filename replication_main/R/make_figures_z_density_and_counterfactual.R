# Setup ----

## Load required libraries ----
library(haven)
library(tidyverse)
library(JWileymisc)

## Load required functions ----
source(
  "replication_main/R/functions/calibrate_counterfactual.R"
)
source(
  "replication_main/R/functions/plot_counterfactual.R"
)

## Load data ----
d_path <- "replication_main/metaenergy_final.dta"
d <- haven::read_dta(d_path)

## Path for figures ----
figure_path <- "replication_main/R/figures/"

## Store data with quality concerns, etc. for robustness checks
d_full_sample <- d

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
## Alternative with winsorization
# wins_lev <- 0.02 # Set winsorization level
# d <- d %>%
#   mutate(sec = JWileymisc::winsorizor(sec, wins_lev, na.rm = TRUE)) %>%
#   mutate(coeffc = JWileymisc::winsorizor(coeffc, wins_lev, na.rm = TRUE))

### Calculating z-statistic values ----
# d <- d %>% mutate(z_stat = abs(coeffc / sec)) # For absolute values only
d <- d %>% mutate(z_stat = coeffc / sec)

# Define significance and calibration thresholds
sig_thresholds <- -c(1.645, 1.96, 2.576)
calibration_threshold <- -5

# Set y_lims
y_lims <- c(0, 0.3)

### For different horizons ----
d$horizond <- factor(
  d$horizond,
  levels = c(0, 1),
  labels = c("short run", "long run")
)

### Figure 1 - Histogram of z-stats for different horizons with 0 line and 5% sign. indicator ----
#### Create 1x2 plot for joint plot
par(mfrow = c(1, 2))
#### Short run ----
breaks <- 750
# Plot
hist(
  d %>% filter(horizond == "short run") %>% pull(z_stat),
  # breaks = breaks,
  breaks = c(
    -10000,
    seq(-10, 10, length.out = 100),
    10000
  ),
  # breaks = c(
  #   -10000,
  #   seq(-10, -2.75, 0.15),
  #   c(-4:-1 * (2.576 - 1.96) / 4 - 1.96),
  #   c(-2:0 * 0.1575 - 1.645),
  #   seq(-1.5, 0, 0.15),
  #   seq(0, 10000, 0.15)
  # ),
  xlim = c(-10, 10),
  ylim = c(0, 0.25),
  probability = TRUE,
  main = "",
  xlab = "z-statistic",
  ylab = "Density",
  col = "lightgray",
  border = "grey"
)
# Add vertical zero line
abline(v = 0, col = "black", lty = 2)
# Add significance indicator at 5% (vertical line -1.960)
abline(v = -1.96, col = "red", lty = 1, lwd = 2)
# Add text for significance indicator
text(-1.96, 0.22, labels = "-1.96 ", adj = c(1, -4), col = "red")
#### Long run ----
# Plot
hist(
  d %>% filter(horizond == "long run") %>% pull(z_stat),
  # breaks = breaks,
  breaks = c(
    -10000,
    seq(-10, 10, length.out = 100),
    10000
  ),
  # breaks = c(-10000, seq(-10, -2.75, 0.15), c(-4:-1*(2.576 - 1.96) / 4 - 1.96), c(-2:0 * 0.1575 - 1.645), seq(-1.5, 0, 0.15), seq(0, 10000, 0.15)),
  xlim = c(-10, 10),
  ylim = c(0, 0.25),
  probability = TRUE,
  main = "",
  xlab = "z-statistic",
  ylab = "Density",
  col = "lightgray",
  border = "grey"
)
# Add vertical zero line
abline(v = 0, col = "black", lty = 2)
# Add significance indicator at 5% (vertical line -1.960)
abline(v = -1.96, col = "red", lty = 1, lwd = 2)
# Add text for significance indicator
text(-1.96, 0.22, labels = "-1.96 ", adj = c(1, -4), col = "red")
# Add subtitle left and right plot
mtext("Short run", side = 3, line = -3, outer = TRUE, at = 0.25, cex = 1.1)
mtext("Long run", side = 3, line = -3, outer = TRUE, at = 0.75, cex = 1.1)
## Add title
# title("Short run (left) vs long run (right)", line = -1, outer = TRUE)
### Save as PDF
dev.copy(
  pdf,
  paste0(
    figure_path,
    "z_stat_densities/",
    "figure_z_hist_short_run_long_run.pdf"
  ),
  width = 10,
  height = 6
)
dev.off()

### Figure B.1 ----
#### Robustness Short- vs Long-run, excluding qconcern and inferior ----
## Drop low quality estimates and estimates judged inferior by study authors
d_no_qconcernd_no_inferior <- d %>%
  filter(qconcernd != 1, preferc != 0)
##### Create 1x2 plot for joint plot
par(mfrow = c(1, 2))
#### Short run ----
breaks <- 750
# Plot
hist(
  d_no_qconcernd_no_inferior %>%
    filter(horizond == "short run") %>%
    pull(z_stat),
  # breaks = breaks,
  breaks = c(
    -10000,
    seq(-10, 10, length.out = 100),
    seq(10, 10000, 0.15)
  ),
  # breaks = c(
  #   -10000,
  #   seq(-10, -2.75, 0.15),
  #   c(-4:-1 * (2.576 - 1.96) / 4 - 1.96),
  #   c(-2:0 * 0.1575 - 1.645),
  #   seq(-1.5, 0, 0.15),
  #   seq(0, 10000, 0.15)
  # ),
  xlim = c(-10, 10),
  ylim = c(0, 0.25),
  probability = TRUE,
  main = "",
  xlab = "z-statistic",
  ylab = "Density",
  col = "lightgray",
  border = "grey"
)
# Add vertical zero line
abline(v = 0, col = "black", lty = 2)
# Add significance indicator at 5% (vertical line -1.960)
abline(v = -1.96, col = "red", lty = 1, lwd = 2)
# Add text for significance indicator
text(-1.96, 0.22, labels = "-1.96 ", adj = c(1, -4), col = "red")
#### Long run ----
# Plot
hist(
  d_no_qconcernd_no_inferior %>%
    filter(horizond == "long run") %>%
    pull(z_stat),
  # breaks = breaks,
  breaks = c(
    -10000,
    seq(-10, 10, length.out = 100),
    seq(10, 10000, 0.15)
  ),
  # breaks = c(-10000, seq(-10, -2.75, 0.15), c(-4:-1*(2.576 - 1.96) / 4 - 1.96), c(-2:0 * 0.1575 - 1.645), seq(-1.5, 0, 0.15), seq(0, 10000, 0.15)),
  xlim = c(-10, 10),
  ylim = c(0, 0.25),
  probability = TRUE,
  main = "",
  xlab = "z-statistic",
  ylab = "Density",
  col = "lightgray",
  border = "grey"
)
# Add vertical zero line
abline(v = 0, col = "black", lty = 2)
# Add significance indicator at 5% (vertical line -1.960)
abline(v = -1.96, col = "red", lty = 1, lwd = 2)
# Add text for significance indicator
text(-1.96, 0.22, labels = "-1.96 ", adj = c(1, -4), col = "red")
# Add subtitle left and right plot
mtext("Short run", side = 3, line = -3, outer = TRUE, at = 0.25, cex = 1.1)
mtext("Long run", side = 3, line = -3, outer = TRUE, at = 0.75, cex = 1.1)
## Add title
# title("Short run (left) vs long run (right) - no quality concerns, no inferior", line = -1, outer = TRUE)
### Save as PDF
dev.copy(
  pdf,
  paste0(
    figure_path,
    "z_stat_densities/",
    "figure_z_hist_short_run_long_run_no_qc_no_inferior.pdf"
  ),
  width = 10,
  height = 6
)
dev.off()

### Figure B.2 - Brodeur et. al. 2020 calibration of counterfactual ----
#### Create 1x2 plot for joint plot
par(mfrow = c(1, 2))
#### Short run ----
# Calibrate
cf_short <- calibrate_counterfactual(
  d %>% filter(horizond == "short run"),
  statistic = "z_stat",
  threshold = calibration_threshold
)
# Plot
plot_counterfactual(
  d %>% filter(horizond == "short run"),
  statistic = "z_stat",
  calibration = cf_short,
  # breaks = breaks,
  breaks = c(
    -10000,
    seq(-10, 10, length.out = 100),
    seq(10, 10000, 0.15)
  ),
  # breaks = c(
  #   -10000,
  #   seq(-10, -2.75, 0.15),
  #   c(-4:-1 * (2.576 - 1.96) / 4 - 1.96),
  #   c(-2:0 * 0.1575 - 1.645),
  #   seq(-1.5, 0, 0.15),
  #   seq(0, 10000, 0.15)
  # ),
  # breaks = c(seq(0, 1.5, 0.15), c(0:2 * 0.1575 + 1.645), c(1:4*(2.576 - 1.96) / 4 + 1.96), 2.576, seq(2.75, 10, 0.15), 10000),
  # breaks = c(seq(0, 10, 0.15), 10000),
  # breaks = c(seq(0, 10, length.out = 50), 10000),
  kernel_adjust = 0.9,
  kernel_bw = 0.3,
  significance = sig_thresholds,
  ylim = y_lims,
  xlims = c(-10, 10),
  show_params = FALSE,
  star_spacing = 0.0075,
  star_buffer = 0,
  star_cex = 1
)
# Add vertical zero line
abline(v = 0, col = "black", lty = 2)
#### Long run ----
# Calibrate
cf_long <- calibrate_counterfactual(
  d %>% filter(horizond == "long run"),
  statistic = "z_stat",
  threshold = calibration_threshold
)
# Plot
plot_counterfactual(
  d %>% filter(horizond == "long run"),
  statistic = "z_stat",
  calibration = cf_long,
  # breaks = breaks,
  breaks = c(
    -10000,
    seq(-10, 10, length.out = 100),
    seq(10, 10000, 0.15)
  ),
  # breaks = c(
  #   -10000,
  #   seq(-10, -2.75, 0.15),
  #   c(-4:-1 * (2.576 - 1.96) / 4 - 1.96),
  #   c(-2:0 * 0.1575 - 1.645),
  #   seq(-1.5, 0, 0.15),
  #   seq(0, 10000, 0.15)
  # ),
  # breaks = c(seq(0, 1.5, 0.15), c(0:2 * 0.1575 + 1.645), c(1:4*(2.576 - 1.96) / 4 + 1.96), 2.576, seq(2.75, 10, 0.15), 10000),
  # breaks = c(seq(0, 10, 0.05), 10000),
  # breaks = c(seq(0, 10, length.out = 50), 10000),
  kernel_adjust = 0.9,
  kernel_bw = 0.3,
  significance = sig_thresholds,
  ylim = y_lims,
  xlims = c(-10, 10),
  show_params = FALSE,
  add_legend = FALSE,
  star_spacing = 0.0075,
  star_buffer = 0,
  star_cex = 1
)
# Add vertical zero line
abline(v = 0, col = "black", lty = 2)
# Add subtitle left and right plot
mtext("Short run", side = 3, line = -3, outer = TRUE, at = 0.25, cex = 1.1)
mtext("Long run", side = 3, line = -3, outer = TRUE, at = 0.75, cex = 1.1)
# Add title
# title("Short run (left) vs long run (right)", line = -1, outer = TRUE)
## Save as PDF
dev.copy(
  pdf,
  paste0(
    figure_path,
    "z_stat_densities/",
    "figure_z_density_and_counterfactual_short_run_long_run.pdf"
  ),
  width = 10,
  height = 6
)
dev.off()

### All - positive and negative ----
par(mfrow = c(1, 1))
# Calibrate
cf_all <- calibrate_counterfactual(
  d,
  statistic = "z_stat",
  threshold = calibration_threshold
)
# Plot
plot_counterfactual(
  d,
  statistic = "z_stat",
  calibration = cf_all,
  breaks = c(
    -10000,
    seq(-10, -2.75, 0.15),
    c(-4:-1 * (2.576 - 1.96) / 4 - 1.96),
    c(-2:0 * 0.1575 - 1.645),
    seq(-1.5, 0, 0.15),
    seq(0, 10000, 0.15)
  ),
  # breaks = c(seq(0, 1.5, 0.15), c(0:2 * 0.1575 + 1.645), c(1:4*(2.576 - 1.96) / 4 + 1.96), 2.576, seq(2.75, 10, 0.15), 10000),
  # breaks = c(seq(0, 10, 0.05), 10000),
  significance = sig_thresholds,
  ylim = y_lims,
  xlims = c(-10, 10),
  show_params = FALSE,
  star_spacing = 0.0075,
  star_buffer = 0,
  star_cex = 1
)
# Add vertical zero line
abline(v = 0, col = "black", lty = 2)

# Add title
title("All effect sizes - positive and negative", line = -1, outer = TRUE)
# Save as PDF
dev.copy(
  pdf,
  paste0(
    figure_path,
    "z_stat_densities/",
    "figure_z_density_and_counterfactual_all.pdf"
  ),
  width = 10,
  height = 10
)
dev.off()


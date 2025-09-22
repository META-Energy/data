# Setup ----

## Load required libraries ----
library(haven)
library(tidyverse)
library(JWileymisc)

## Load required functions ----
source("/Users/franzprante/GitHub/MORPEP/META_CMP/data/analysis/R/calibrate_counterfactual.R")
source("/Users/franzprante/GitHub/MORPEP/META_CMP/data/analysis/R/plot_counterfactual.R")

## Load data ----
d_path <- "~/Nextcloud/project-vwl4makro/09_Forschung/Drittmittel/DZ2022/WP2_Meta_energy/estimation/metaenergy_final.dta" 
d <- haven::read_dta(d_path)

## Path for figures ----
figure_path <- "~/Nextcloud/project-vwl4makro/09_Forschung/Drittmittel/DZ2022/WP2_Meta_energy/estimation/R Franz/working_paper_1/figures/"

## Store data with quality concerns, etc. for robustness checks
d_full_sample <- d

## Drop estimates for which we only have assumed precision measures based on ----
## "significance stars".
d <- d %>%
  filter(!(is.na(se_orig) &
         is.na(t_orig) &
         p_val_calc %in% c(0.001, 0.01, 0.05, 0.1, 0.5))
         )

# ## Winsorization ----
wins_lev <- 0.02 # Set winsorization level
d <- d %>%
  mutate(sec = JWileymisc::winsorizor(sec, wins_lev, na.rm = TRUE)) %>%
  mutate(coeffc = JWileymisc::winsorizor(coeffc, wins_lev, na.rm = TRUE))

## Data prep ----

### Calculating z-statistic values ----
# d <- d %>% mutate(z_stat = abs(coeffc / sec)) # For absolute values only
d <- d %>% mutate(z_stat = coeffc / sec)

# Define significance and calibration thresholds
sig_thresholds <- -c(1.645, 1.96, 2.576)
calibration_threshold <- -5

# Set y_lims
y_lims <- c(0, 0.3)

### All - positive and negative ----
par(mfrow = c(1, 1))
# Calibrate
cf_all <- calibrate_counterfactual(d,
                               statistic = "z_stat",
                               threshold = calibration_threshold)
# Plot
plot_counterfactual(d,
                    statistic = "z_stat",
                    calibration = cf_all,
                    breaks = c(-10000, seq(-10, -2.75, 0.15), c(-4:-1*(2.576 - 1.96) / 4 - 1.96), c(-2:0 * 0.1575 - 1.645), seq(-1.5, 0, 0.15), seq(0, 10000, 0.15)),
                    # breaks = c(seq(0, 1.5, 0.15), c(0:2 * 0.1575 + 1.645), c(1:4*(2.576 - 1.96) / 4 + 1.96), 2.576, seq(2.75, 10, 0.15), 10000),
                    # breaks = c(seq(0, 10, 0.05), 10000),
                    significance = sig_thresholds,
                    ylim = y_lims,
                    xlims = c(-10, 10),
                    show_params = FALSE,
                    star_spacing = 0.0075,
                    star_buffer = 0,
                    star_cex = 1)
# Add vertical zero line
abline(v = 0, col = "black", lty = 2)

# Add title
title("All effect sizes - positive and negative", line = -1, outer = TRUE)
# Save as PDF
dev.copy(pdf, paste0(figure_path, "z_stat_densities/", "figure_z_density_and_counterfactual_all.pdf"), 
         width = 10, 
         height = 10)
dev.off()

### For different horizons ----
d$horizond <- factor(d$horizond, levels = c(1, 2), labels = c("short run", "long run"))
#### Create 1x2 plot for joint plot
par(mfrow = c(1, 2))
#### Short run ----
# Calibrate
cf_short <- calibrate_counterfactual(d %>% filter(horizond == "short run"),
                               statistic = "z_stat",
                               threshold = calibration_threshold)
# Plot
plot_counterfactual(d %>% filter(horizond == "short run"),
                    statistic = "z_stat",
                    calibration = cf_short,
                    breaks = c(-10000, seq(-10, -2.75, 0.15), c(-4:-1*(2.576 - 1.96) / 4 - 1.96), c(-2:0 * 0.1575 - 1.645), seq(-1.5, 0, 0.15), seq(0, 10000, 0.15)),
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
                    star_cex = 1)
# Add vertical zero line
abline(v = 0, col = "black", lty = 2)
#### Long run ----
# Calibrate
cf_long <- calibrate_counterfactual(d %>% filter(horizond == "long run"),
                               statistic = "z_stat",
                               threshold = calibration_threshold)
# Plot
plot_counterfactual(d %>% filter(horizond == "long run"),
                    statistic = "z_stat",
                    calibration = cf_long,
                    breaks = c(-10000, seq(-10, -2.75, 0.15), c(-4:-1*(2.576 - 1.96) / 4 - 1.96), c(-2:0 * 0.1575 - 1.645), seq(-1.5, 0, 0.15), seq(0, 10000, 0.15)),
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
                    star_cex = 1)
# Add vertical zero line
abline(v = 0, col = "black", lty = 2)
# Add subtitle left and right plot 
mtext("Short run", side = 3, line = -3, outer = TRUE, at = 0.25, cex = 1.1)
mtext("Long run", side = 3, line = -3, outer = TRUE, at = 0.75, cex = 1.1)
# Add title
# title("Short run (left) vs long run (right)", line = -1, outer = TRUE)
## Save as PDF
dev.copy(pdf, paste0(figure_path, "z_stat_densities/", "figure_z_density_and_counterfactual_short_run_long_run.pdf"), 
         width = 10, 
         height = 6)
dev.off()
#### With smaller bins ----
par(mfrow = c(1, 2))
plot_counterfactual(d %>% filter(horizond == "short run"),
                    statistic = "z_stat",
                    calibration = cf_short,
                    breaks = c(-10000, seq(-10, -2.75, 0.15), c(-4:-1*(2.576 - 1.96) / 4 - 1.96), c(-2:0 * 0.1575 - 1.645), seq(-1.5, 0, 0.15), seq(0, 10000, 0.15)),
                    # breaks = c(seq(0, 10, 0.05), 10000),
                    # breaks = c(seq(0, 10, 0.02), 10000),
                    kernel_adjust = 0.5,
                    kernel_bw = 0.2,
                    significance = sig_thresholds,
                    ylim = y_lims,
                    xlims = c(-10, 10),
                    show_params = FALSE,
                    star_spacing = 0.0075,
                    star_buffer = 0,
                    star_cex = 1)
plot_counterfactual(d %>% filter(horizond == "long run"),
                    statistic = "z_stat",
                    calibration = cf_long,
                    breaks = c(-10000, seq(-10, -2.75, 0.15), c(-4:-1*(2.576 - 1.96) / 4 - 1.96), c(-2:0 * 0.1575 - 1.645), seq(-1.5, 0, 0.15), seq(0, 10000, 0.15)),
                    # breaks = c(seq(0, 10, 0.05), 10000),
                    # breaks = c(seq(0, 10, 0.02), 10000),
                    significance = sig_thresholds,
                    ylim = y_lims,
                    xlims = c(-10, 10),
                    show_params = FALSE,
                    add_legend = FALSE,
                    star_spacing = 0.0075,
                    star_buffer = 0,
                    star_cex = 1)
# Add vertical zero line
abline(v = 0, col = "black", lty = 2)
# Add subtitle left and right plot 
mtext("Short run", side = 3, line = -3, outer = TRUE, at = 0.25, cex = 1.1)
mtext("Long run", side = 3, line = -3, outer = TRUE, at = 0.75, cex = 1.1)
# Add title
# title("Short run (left) vs long run (right) - smaller bins, lower density smoothing", line = -1, outer = TRUE)
## Save as PDF
dev.copy(pdf, paste0(figure_path, "z_stat_densities/", "figure_z_density_and_counterfactual_short_run_long_run_no_cf.pdf"), 
         width = 10, 
         height = 6)
dev.off()

#### Robustness Short- vs Long-run, excluding qconcern and inferior ----
## Drop low quality estimates and estimates judged inferior by study authors
d_no_qconcernd_no_inferior <- d %>%
  filter(qconcernd != 1,
         preferc != -1
        )
##### Create 1x2 plot for joint plot
par(mfrow = c(1, 2))
##### Short run ----
## Calibrate
cf_short <- calibrate_counterfactual(d_no_qconcernd_no_inferior %>% filter(horizond == "short run"),
                                         statistic = "z_stat",
                                         threshold = calibration_threshold)
## Plot
plot_counterfactual(d_no_qconcernd_no_inferior %>% filter(horizond == "short run"),
                    statistic = "z_stat",
                    calibration = cf_short,
                    breaks = c(-10000, seq(-10, -2.75, 0.15), c(-4:-1*(2.576 - 1.96) / 4 - 1.96), c(-2:0 * 0.1575 - 1.645), seq(-1.5, 0, 0.15), seq(0, 10000, 0.15)),
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
                    star_cex = 1)
# Add vertical zero line
abline(v = 0, col = "black", lty = 2)
##### Long run ----
## Calibrate
cf_long <- calibrate_counterfactual(d_no_qconcernd_no_inferior %>% filter(horizond == "long run"),
                                        statistic = "z_stat",
                                        threshold = calibration_threshold)
## Plot
plot_counterfactual(d_no_qconcernd_no_inferior %>% filter(horizond == "long run"),
                    statistic = "z_stat",
                    calibration = cf_long,
                    breaks = c(-10000, seq(-10, -2.75, 0.15), c(-4:-1*(2.576 - 1.96) / 4 - 1.96), c(-2:0 * 0.1575 - 1.645), seq(-1.5, 0, 0.15), seq(0, 10000, 0.15)),
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
                    star_cex = 1)
# Add vertical zero line
abline(v = 0, col = "black", lty = 2)
# Add subtitle left and right plot 
mtext("Short run", side = 3, line = -3, outer = TRUE, at = 0.25, cex = 1.1)
mtext("Long run", side = 3, line = -3, outer = TRUE, at = 0.75, cex = 1.1)
## Add title
# title("Short run (left) vs long run (right) - no quality concerns, no inferior", line = -1, outer = TRUE)
### Save as PDF
dev.copy(pdf, paste0(figure_path, "z_stat_densities/", "figure_z_density_and_counterfactual_short_run_long_run_no_qc_no_inferior.pdf"), 
         width = 10, 
         height = 6)
dev.off()


#### Robustness Short- vs Long-run, negative effects based on d_full_sample ----
d_full_sample$horizond <- factor(d_full_sample$horizond, levels = c(1, 2), labels = c("short run", "long run"))
d_full_sample$z_stat <- d_full_sample$coeffc / d_full_sample$sec
d_full_sample <- d_full_sample
#### Create 1x2 plot for joint plot
par(mfrow = c(1, 2))
##### Short run ----
# Calibrate
cf_full_sample_short <- calibrate_counterfactual(d_full_sample %>% filter(horizond == "short run"),
                                                 statistic = "z_stat",
                                                 threshold = calibration_threshold)
# Plot
plot_counterfactual(d_full_sample %>% filter(horizond == "short run"),
                    statistic = "z_stat",
                    calibration = cf_full_sample_short,
                    breaks = c(-10000, seq(-10, -2.75, 0.15), c(-4:-1*(2.576 - 1.96) / 4 - 1.96), c(-2:0 * 0.1575 - 1.645), seq(-1.5, 0, 0.15), seq(0, 10000, 0.15)),
                    # breaks = c(seq(0, 1.5, 0.15), c(0:2 * 0.1575 + 1.645), c(1:4*(2.576 - 1.96) / 4 + 1.96), 2.576, seq(2.75, 10, 0.15), 10000),
                    # breaks = c(seq(0, 10, 0.05), 10000),
                    # breaks = c(seq(0, 10, length.out = 50), 10000),
                    kernel_adjust = 0.9,
                    kernel_bw = 0.3,
                    significance = sig_thresholds,
                    ylim = y_lims,
                    xlims = c(-10, 10),
                    show_params = FALSE,
                    star_spacing = 0.0075,
                    star_buffer = 0,
                    star_cex = 1)
# Add vertical zero line
abline(v = 0, col = "black", lty = 2)
##### Long run ----
# Calibrate
cf_full_sample_long <- calibrate_counterfactual(d_full_sample %>% filter(horizond == "long run"),
                                                statistic = "z_stat",
                                                threshold = calibration_threshold)
# Plot
plot_counterfactual(d_full_sample %>% filter(horizond == "long run"),
                    statistic = "z_stat",
                    calibration = cf_full_sample_long,
                    breaks = c(-10000, seq(-10, -2.75, 0.15), c(-4:-1*(2.576 - 1.96) / 4 - 1.96), c(-2:0 * 0.1575 - 1.645), seq(-1.5, 0, 0.15), seq(0, 10000, 0.15)),
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
                    star_cex = 1)
# Add vertical zero line
abline(v = 0, col = "black", lty = 2)
# Add subtitle left and right plot 
mtext("Short run", side = 3, line = -3, outer = TRUE, at = 0.25, cex = 1.1)
mtext("Long run", side = 3, line = -3, outer = TRUE, at = 0.75, cex = 1.1)
# Add title
# title("Short run (left) vs long run (right) - no winsorization, no filters", line = -1, outer = TRUE)
## Save as PDF
dev.copy(pdf, paste0(figure_path, "z_stat_densities/", "figure_z_density_and_counterfactual_short_run_long_run_full_sample.pdf"), 
         width = 10, 
         height = 6)
dev.off()

#### Robustness Short- vs Long-run without counterfactuals, without threshold-based precision, negative effects, no other filters ----
d_full_sample_no_thresh <- d_full_sample %>%
  filter(!(is.na(se_orig) &
             is.na(t_orig) &
             p_val_calc %in% c(0.001, 0.01, 0.05, 0.1, 0.5))
  )
#### Create 1x2 plot for joint plot
par(mfrow = c(1, 2))
##### Short run ----
# Calibrate
cf_full_sample_short <- calibrate_counterfactual(d_full_sample_no_thresh %>% filter(horizond == "short run"),
                               statistic = "z_stat",
                               threshold = calibration_threshold)
# Plot
plot_counterfactual(d_full_sample_no_thresh %>% filter(horizond == "short run"),
                    statistic = "z_stat",
                    calibration = cf_full_sample_short,
                    breaks = c(-10000, seq(-10, -2.75, 0.15), c(-4:-1*(2.576 - 1.96) / 4 - 1.96), c(-2:0 * 0.1575 - 1.645), seq(-1.5, 0, 0.15), seq(0, 10000, 0.15)),
                    # breaks = c(seq(0, 1.5, 0.15), c(0:2 * 0.1575 + 1.645), c(1:4*(2.576 - 1.96) / 4 + 1.96), 2.576, seq(2.75, 10, 0.15), 10000),
                    # breaks = c(seq(0, 10, 0.05), 10000),
                    # breaks = c(seq(0, 10, length.out = 50), 10000),
                    kernel_adjust = 0.9,
                    kernel_bw = 0.3,
                    significance = sig_thresholds,
                    ylim = y_lims,
                    xlims = c(-10, 10),
                    show_params = FALSE,
                    star_spacing = 0.0075,
                    star_buffer = 0,
                    star_cex = 1)
# Add vertical zero line
abline(v = 0, col = "black", lty = 2)
##### Long run ----
# Calibrate
cf_full_sample_long <- calibrate_counterfactual(d_full_sample_no_thresh %>% filter(horizond == "long run"),
                               statistic = "z_stat",
                               threshold = calibration_threshold)
# Plot
plot_counterfactual(d_full_sample_no_thresh %>% filter(horizond == "long run"),
                    statistic = "z_stat",
                    calibration = cf_full_sample_long,
                    breaks = c(-10000, seq(-10, -2.75, 0.15), c(-4:-1*(2.576 - 1.96) / 4 - 1.96), c(-2:0 * 0.1575 - 1.645), seq(-1.5, 0, 0.15), seq(0, 10000, 0.15)),
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
                    star_cex = 1)
# Add vertical zero line
abline(v = 0, col = "black", lty = 2)
# Add subtitle left and right plot 
mtext("Short run", side = 3, line = -3, outer = TRUE, at = 0.25, cex = 1.1)
mtext("Long run", side = 3, line = -3, outer = TRUE, at = 0.75, cex = 1.1)
# # Add title
# title("Short run (left) vs long run (right) - no winsorization, no threshold-based precision measures, no other filters", line = -1, outer = TRUE)
## Save as PDF
dev.copy(pdf, paste0(figure_path, "z_stat_densities/", "figure_z_density_and_counterfactual_short_run_long_run_full_sample_no_thresh.pdf"), 
         width = 10, 
         height = 6)
dev.off()

### Top journals vs other publications ----
d$topjourd <- factor(d$topjourd, levels = c(0, 1), labels = c("Other", "Top journal"))
#### Create 1x2 plot for joint plot
par(mfrow = c(1, 2))
#### Other ----
# Calibrate
cf_other <- calibrate_counterfactual(d %>% filter(topjourd == "Other"),
                               statistic = "z_stat",
                               threshold = calibration_threshold)
# Plot
plot_counterfactual(d %>% filter(topjourd == "Other"),
                    statistic = "z_stat",
                    calibration = cf_other,
                    breaks = c(-10000, seq(-10, -2.75, 0.15), c(-4:-1*(2.576 - 1.96) / 4 - 1.96), c(-2:0 * 0.1575 - 1.645), seq(-1.5, 0, 0.15), seq(0, 10000, 0.15)),
                    # breaks = c(seq(0, 1.5, 0.15), c(0:2 * 0.1575 + 1.645), c(1:4*(2.576 - 1.96) / 4 + 1.96), 2.576, seq(2.75, 10, 0.15), 10000),
                    # breaks = c(seq(0, 10, 0.05), 10000),
                    # breaks = c(seq(0, 10, length.out = 50), 10000),
                    significance = sig_thresholds,
                    ylim = y_lims,
                    xlims = c(-10, 10),
                    show_params = FALSE,
                    star_spacing = 0.0075,
                    star_buffer = 0,
                    star_cex = 1)
#### Top journal ----
# Calibrate
cf_top_journal <- calibrate_counterfactual(d %>% filter(topjourd == "Top journal"),
                               statistic = "z_stat",
                               threshold = calibration_threshold)
# Plot
plot_counterfactual(d %>% filter(topjourd == "Top journal"),
                    statistic = "z_stat",
                    calibration = cf_top_journal,
                    breaks = c(-10000, seq(-10, -2.75, 0.15), c(-4:-1*(2.576 - 1.96) / 4 - 1.96), c(-2:0 * 0.1575 - 1.645), seq(-1.5, 0, 0.15), seq(0, 10000, 0.15)),
                    # breaks = c(seq(0, 1.5, 0.15), c(0:2 * 0.1575 + 1.645), c(1:4*(2.576 - 1.96) / 4 + 1.96), 2.576, seq(2.75, 10, 0.15), 10000),
                    # breaks = c(seq(0, 10, 0.05), 10000),
                    # breaks = c(seq(0, 10, length.out = 50), 10000),
                    significance = sig_thresholds,
                    ylim = y_lims,
                    xlims = c(-10, 10),
                    show_params = FALSE,
                    add_legend = FALSE,
                    star_spacing = 0.0075,
                    star_buffer = 0,
                    star_cex = 1)
# Add vertical zero line
abline(v = 0, col = "black", lty = 2)
# Add title
title("Other (left) vs top journal (right)", line = -1, outer = TRUE)
## Save as PDF
dev.copy(pdf, paste0(figure_path, "z_stat_densities/", "figure_z_density_and_counterfactual_other_top_journal.pdf"), 
         width = 10, 
         height = 6)
dev.off()

### Micro vs macro ----
d$micromacro <- factor(ifelse(d$micro == 1, 1, ifelse(d$macro == 1, 2, NA)),
                           levels = c(1, 2),
                           labels = c("micro", "macro"))
#### Create 1x2 plot for joint plot
par(mfrow = c(1, 2))
#### Micro ----
# Calibrate
cf_micro <- calibrate_counterfactual(d %>% filter(micromacro == "micro"),
                               statistic = "z_stat",
                               threshold = calibration_threshold)
# Plot
plot_counterfactual(d %>% filter(micromacro == "micro"),
                    statistic = "z_stat",
                    calibration = cf_micro,
                    breaks = c(-10000, seq(-10, -2.75, 0.15), c(-4:-1*(2.576 - 1.96) / 4 - 1.96), c(-2:0 * 0.1575 - 1.645), seq(-1.5, 0, 0.15), seq(0, 10000, 0.15)),
                    # breaks = c(seq(0, 1.5, 0.15), c(0:2 * 0.1575 + 1.645), c(1:4*(2.576 - 1.96) / 4 + 1.96), 2.576, seq(2.75, 10, 0.15), 10000),
                    # breaks = c(seq(0, 10, 0.05), 10000),
                    # breaks = c(seq(0, 10, length.out = 50), 10000),
                    significance = sig_thresholds,
                    ylim = y_lims,
                    xlims = c(-10, 10),
                    show_params = FALSE,
                    star_spacing = 0.0075,
                    star_buffer = 0,
                    star_cex = 1)
# Add vertical zero line
abline(v = 0, col = "black", lty = 2)
#### Macro ----
# Calibrate
cf_macro <- calibrate_counterfactual(d %>% filter(micromacro == "macro"),
                               statistic = "z_stat",
                               threshold = calibration_threshold)
# Plot
plot_counterfactual(d %>% filter(micromacro == "macro"),
                    statistic = "z_stat",
                    calibration = cf_macro,
                    breaks = c(-10000, seq(-10, -2.75, 0.15), c(-4:-1*(2.576 - 1.96) / 4 - 1.96), c(-2:0 * 0.1575 - 1.645), seq(-1.5, 0, 0.15), seq(0, 10000, 0.15)),
                    # breaks = c(seq(0, 1.5, 0.15), c(0:2 * 0.1575 + 1.645), c(1:4*(2.576 - 1.96) / 4 + 1.96), 2.576, seq(2.75, 10, 0.15), 10000),
                    # breaks = c(seq(0, 10, 0.05), 10000),
                    # breaks = c(seq(0, 10, length.out = 50), 10000),
                    significance = sig_thresholds,
                    ylim = y_lims,
                    xlims = c(-10, 10),
                    show_params = FALSE,
                    add_legend = FALSE,
                    star_spacing = 0.0075,
                    star_buffer = 0,
                    star_cex = 1)
# Add vertical zero line
abline(v = 0, col = "black", lty = 2)
# Add title
title("Micro (left) vs macro (right)", line = -1, outer = TRUE)
## Save as PDF
dev.copy(pdf, paste0(figure_path, "z_stat_densities/", "figure_z_density_and_counterfactual_micro_macro.pdf"), 
         width = 10, 
         height = 6)
dev.off()

### Byproduct ----
par(mfrow = c(1, 2))
unique(d$byproduct)
##### 0 ----
# Calibrate
cf_byproduct_0 <- calibrate_counterfactual(d %>% filter(byproduct == 0),
                                         statistic = "z_stat",
                                         threshold = calibration_threshold)
# Plot
plot_counterfactual(d %>% filter(byproduct == 0),
                    statistic = "z_stat",
                    calibration = cf_byproduct_0,
                    breaks = c(-10000, seq(-10, -2.75, 0.15), c(-4:-1*(2.576 - 1.96) / 4 - 1.96), c(-2:0 * 0.1575 - 1.645), seq(-1.5, 0, 0.15), seq(0, 10000, 0.15)),
                    # breaks = c(seq(0, 1.5, 0.15), c(0:2 * 0.1575 + 1.645), c(1:4*(2.576 - 1.96) / 4 + 1.96), 2.576, seq(2.75, 10, 0.15), 10000),
                    # breaks = c(seq(0, 10, 0.05), 10000),
                    # breaks = c(seq(0, 10, length.out = 50), 10000),
                    significance = sig_thresholds,
                    ylim = y_lims,
                    xlims = c(-10, 10),
                    show_params = FALSE,
                    star_spacing = 0.0075,
                    star_buffer = 0,
                    star_cex = 1)
# Add vertical zero line
abline(v = 0, col = "black", lty = 2)
##### 1 ----
# Calibrate
cf_byproduct_1 <- calibrate_counterfactual(d %>% filter(byproduct == 1),
                                         statistic = "z_stat",
                                         threshold = calibration_threshold)
# Plot
plot_counterfactual(d %>% filter(byproduct == 1),
                    statistic = "z_stat",
                    calibration = cf_byproduct_1,
                    breaks = c(-10000, seq(-10, -2.75, 0.15), c(-4:-1*(2.576 - 1.96) / 4 - 1.96), c(-2:0 * 0.1575 - 1.645), seq(-1.5, 0, 0.15), seq(0, 10000, 0.15)),
                    # breaks = c(seq(0, 1.5, 0.15), c(0:2 * 0.1575 + 1.645), c(1:4*(2.576 - 1.96) / 4 + 1.96), 2.576, seq(2.75, 10, 0.15), 10000),
                    # breaks = c(seq(0, 10, 0.05), 10000),
                    # breaks = c(seq(0, 10, length.out = 50), 10000),
                    significance = sig_thresholds,
                    ylim = y_lims,
                    xlims = c(-10, 10),
                    show_params = FALSE,
                    add_legend = FALSE,
                    star_spacing = 0.0075,
                    star_buffer = 0,
                    star_cex = 1)
# Add vertical zero line
abline(v = 0, col = "black", lty = 2)
# Add title
title("Byproduct = 0 (left) vs Byproduct = 1 (right)", line = -1, outer = TRUE)
## Save as PDF
dev.copy(pdf, paste0(figure_path, "z_stat_densities/", "figure_z_density_and_counterfactual_byproduct.pdf"), 
         width = 10, 
         height = 6)
dev.off()

### Data dimension ----
d$datadimc <- factor(d$datadimc, levels = c(1, 2, 3), 
                         labels = c("crossec", "timeseries", "panel"))
#### Create 1x3 plot for joint plot
par(mfrow = c(1, 3))
#### Crossec ----
# Calibrate
cf_crossec <- calibrate_counterfactual(d %>% filter(datadimc == "crossec"),
                               statistic = "z_stat",
                               threshold = calibration_threshold)
# Plot
plot_counterfactual(d %>% filter(datadimc == "crossec"),
                    statistic = "z_stat",
                    calibration = cf_crossec,
                    breaks = c(-10000, seq(-10, -2.75, 0.15), c(-4:-1*(2.576 - 1.96) / 4 - 1.96), c(-2:0 * 0.1575 - 1.645), seq(-1.5, 0, 0.15), seq(0, 10000, 0.15)),
                    # breaks = c(seq(0, 1.5, 0.15), c(0:2 * 0.1575 + 1.645), c(1:4*(2.576 - 1.96) / 4 + 1.96), 2.576, seq(2.75, 10, 0.15), 10000),
                    # breaks = c(seq(0, 10, 0.05), 10000),
                    # breaks = c(seq(0, 10, length.out = 50), 10000),
                    significance = sig_thresholds,
                    ylim = y_lims,
                    xlims = c(-10, 10),
                    show_params = FALSE,
                    star_spacing = 0.0075,
                    star_buffer = 0,
                    star_cex = 1)
# Add vertical zero line
abline(v = 0, col = "black", lty = 2)
#### Timeseries ----
# Calibrate
cf_timeseries <- calibrate_counterfactual(d %>% filter(datadimc == "timeseries"),
                               statistic = "z_stat",
                               threshold = calibration_threshold)
# Plot
plot_counterfactual(d %>% filter(datadimc == "timeseries"),
                    statistic = "z_stat",
                    calibration = cf_timeseries,
                    breaks = c(-10000, seq(-10, -2.75, 0.15), c(-4:-1*(2.576 - 1.96) / 4 - 1.96), c(-2:0 * 0.1575 - 1.645), seq(-1.5, 0, 0.15), seq(0, 10000, 0.15)),
                    # breaks = c(seq(0, 1.5, 0.15), c(0:2 * 0.1575 + 1.645), c(1:4*(2.576 - 1.96) / 4 + 1.96), 2.576, seq(2.75, 10, 0.15), 10000),
                    # breaks = c(seq(0, 10, 0.05), 10000),
                    # breaks = c(seq(0, 10, length.out = 50), 10000),
                    significance = sig_thresholds,
                    ylim = y_lims,
                    xlims = c(-10, 10),
                    show_params = FALSE,
                    add_legend = FALSE,
                    star_spacing = 0.0075,
                    star_buffer = 0,
                    star_cex = 1)
# Add vertical zero line
abline(v = 0, col = "black", lty = 2)
#### Panel ----
# Calibrate
cf_panel <- calibrate_counterfactual(d %>% filter(datadimc == "panel"),
                               statistic = "z_stat",
                               threshold = calibration_threshold)
# Plot
plot_counterfactual(d %>% filter(datadimc == "panel"),
                    statistic = "z_stat",
                    calibration = cf_panel,
                    breaks = c(-10000, seq(-10, -2.75, 0.15), c(-4:-1*(2.576 - 1.96) / 4 - 1.96), c(-2:0 * 0.1575 - 1.645), seq(-1.5, 0, 0.15), seq(0, 10000, 0.15)),
                    # breaks = c(seq(0, 1.5, 0.15), c(0:2 * 0.1575 + 1.645), c(1:4*(2.576 - 1.96) / 4 + 1.96), 2.576, seq(2.75, 10, 0.15), 10000),
                    # breaks = c(seq(0, 10, 0.05), 10000),
                    # breaks = c(seq(0, 10, length.out = 50), 10000),
                    significance = sig_thresholds,
                    ylim = y_lims,
                    xlims = c(-10, 10),
                    show_params = FALSE,
                    add_legend = FALSE,
                    star_spacing = 0.0075,
                    star_buffer = 0,
                    star_cex = 1)
# Add vertical zero line
abline(v = 0, col = "black", lty = 2)
# Add title
title("Crossec (left) vs Timeseries (middle) vs Panel (right)", line = -1, outer = TRUE)
## Save as PDF
dev.copy(pdf, paste0(figure_path, "z_stat_densities/", "figure_z_density_and_counterfactual_crossec_timeseries_panel.pdf"), 
         width = 10, 
         height = 6)
dev.off()

### Identified ----
par(mfrow = c(1, 2))
# Warning: There still seem to be some NA values in the dataset for `identifd`.
d$identifd <- factor(d$identifd, levels = c(0, 1), 
                         labels = c("not identified", "identified"))
#### Not identified ----
# Calibrate
cf_not_identified <- calibrate_counterfactual(d %>% filter(identifd == "not identified"),
                               statistic = "z_stat",
                               threshold = calibration_threshold)
# Plot
plot_counterfactual(d %>% filter(identifd == "not identified"),
                    statistic = "z_stat",
                    calibration = cf_not_identified,
                    breaks = c(-10000, seq(-10, -2.75, 0.15), c(-4:-1*(2.576 - 1.96) / 4 - 1.96), c(-2:0 * 0.1575 - 1.645), seq(-1.5, 0, 0.15), seq(0, 10000, 0.15)),
                    # breaks = c(seq(0, 1.5, 0.15), c(0:2 * 0.1575 + 1.645), c(1:4*(2.576 - 1.96) / 4 + 1.96), 2.576, seq(2.75, 10, 0.15), 10000),
                    # breaks = c(seq(0, 10, 0.05), 10000),
                    # breaks = c(seq(0, 10, length.out = 50), 10000),
                    significance = sig_thresholds,
                    ylim = y_lims,
                    xlims = c(-10, 10),
                    show_params = FALSE,
                    star_spacing = 0.0075,
                    star_buffer = 0,
                    star_cex = 1)
# Add vertical zero line
abline(v = 0, col = "black", lty = 2)
#### Identified ----
# Calibrate
cf_identified <- calibrate_counterfactual(d %>% filter(identifd == "identified"),
                               statistic = "z_stat",
                               threshold = calibration_threshold)
# Plot
plot_counterfactual(d %>% filter(identifd == "identified"),
                    statistic = "z_stat",
                    calibration = cf_identified,
                    breaks = c(-10000, seq(-10, -2.75, 0.15), c(-4:-1*(2.576 - 1.96) / 4 - 1.96), c(-2:0 * 0.1575 - 1.645), seq(-1.5, 0, 0.15), seq(0, 10000, 0.15)),
                    # breaks = c(seq(0, 1.5, 0.15), c(0:2 * 0.1575 + 1.645), c(1:4*(2.576 - 1.96) / 4 + 1.96), 2.576, seq(2.75, 10, 0.15), 10000),
                    # breaks = c(seq(0, 10, 0.05), 10000),
                    # breaks = c(seq(0, 10, length.out = 50), 10000),
                    significance = sig_thresholds,
                    ylim = y_lims,
                    xlims = c(-10, 10),
                    show_params = FALSE,
                    add_legend = FALSE,
                    star_spacing = 0.0075,
                    star_buffer = 0,
                    star_cex = 1)
# Add vertical zero line
abline(v = 0, col = "black", lty = 2)
# Add title
title("Not identified (left) vs Identified (right)", line = -1, outer = TRUE)
## Save as PDF
dev.copy(pdf, paste0(figure_path, "z_stat_densities/", "figure_z_density_and_counterfactual_not_identified_identified.pdf"), 
         width = 10, 
         height = 6)
dev.off()

#### Identification strategies ----
d$identifc <- factor(d$identifc, levels = c(0, 1, 2), 
                         labels = c("None", "IV/GMM", "DiD, experiment, etc."))
#### Create 1x3 plot for joint plot
par(mfrow = c(1, 3))
#### None ----
# Calibrate
cf_none <- calibrate_counterfactual(d %>% filter(identifc == "None"),
                               statistic = "z_stat",
                               threshold = calibration_threshold)
# Plot
plot_counterfactual(d %>% filter(identifc == "None"),
                    statistic = "z_stat",
                    calibration = cf_none,
                    breaks = c(-10000, seq(-10, -2.75, 0.15), c(-4:-1*(2.576 - 1.96) / 4 - 1.96), c(-2:0 * 0.1575 - 1.645), seq(-1.5, 0, 0.15), seq(0, 10000, 0.15)),
                    # breaks = c(seq(0, 1.5, 0.15), c(0:2 * 0.1575 + 1.645), c(1:4*(2.576 - 1.96) / 4 + 1.96), 2.576, seq(2.75, 10, 0.15), 10000),
                    # breaks = c(seq(0, 10, 0.05), 10000),
                    # breaks = c(seq(0, 10, length.out = 50), 10000),
                    significance = sig_thresholds,
                    ylim = y_lims,
                    xlims = c(-10, 10),
                    show_params = FALSE,
                    star_spacing = 0.0075,
                    star_buffer = 0,
                    star_cex = 1)
# Add vertical zero line
abline(v = 0, col = "black", lty = 2)
#### IV/GMM ----
# Calibrate
cf_iv_gmm <- calibrate_counterfactual(d %>% filter(identifc == "IV/GMM"),
                               statistic = "z_stat",
                               threshold = calibration_threshold)
# Plot
plot_counterfactual(d %>% filter(identifc == "IV/GMM"),
                    statistic = "z_stat",
                    calibration = cf_iv_gmm,
                    breaks = c(-10000, seq(-10, -2.75, 0.15), c(-4:-1*(2.576 - 1.96) / 4 - 1.96), c(-2:0 * 0.1575 - 1.645), seq(-1.5, 0, 0.15), seq(0, 10000, 0.15)),
                    # breaks = c(seq(0, 1.5, 0.15), c(0:2 * 0.1575 + 1.645), c(1:4*(2.576 - 1.96) / 4 + 1.96), 2.576, seq(2.75, 10, 0.15), 10000),
                    # breaks = c(seq(0, 10, 0.05), 10000),
                    # breaks = c(seq(0, 10, length.out = 50), 10000),
                    significance = sig_thresholds,
                    ylim = y_lims,
                    xlims = c(-10, 10),
                    show_params = FALSE,
                    add_legend = FALSE,
                    star_spacing = 0.0075,
                    star_buffer = 0,
                    star_cex = 1)
# Add vertical zero line
abline(v = 0, col = "black", lty = 2)
#### DiD, experiment, etc. ----
# Calibrate
cf_did_exp <- calibrate_counterfactual(d %>% filter(identifc == "DiD, experiment, etc."),
                               statistic = "z_stat",
                               threshold = calibration_threshold)
# Plot
plot_counterfactual(d %>% filter(identifc == "DiD, experiment, etc."),
                    statistic = "z_stat",
                    calibration = cf_did_exp,
                    breaks = c(-10000, seq(-10, -2.75, 0.15), c(-4:-1*(2.576 - 1.96) / 4 - 1.96), c(-2:0 * 0.1575 - 1.645), seq(-1.5, 0, 0.15), seq(0, 10000, 0.15)),
                    # breaks = c(seq(0, 1.5, 0.15), c(0:2 * 0.1575 + 1.645), c(1:4*(2.576 - 1.96) / 4 + 1.96), 2.576, seq(2.75, 10, 0.15), 10000),
                    # breaks = c(seq(0, 10, 0.05), 10000),
                    # breaks = c(seq(0, 10, length.out = 50), 10000),
                    significance = sig_thresholds,
                    ylim = y_lims,
                    xlims = c(-10, 10),
                    show_params = FALSE,
                    add_legend = FALSE,
                    star_spacing = 0.0075,
                    star_buffer = 0,
                    star_cex = 1)
# Add vertical zero line
abline(v = 0, col = "black", lty = 2)
# Add title
title("None (left) vs IV/GMM (middle) vs DiD, experiment, etc. (right)", line = -1, outer = TRUE)
## Save as PDF
dev.copy(pdf, paste0(figure_path, "z_stat_densities/", "figure_z_density_and_counterfactual_identification_strategies.pdf"), 
         width = 10, 
         height = 6)
dev.off()

### Author judgment ----
d$preferc <- factor(d$preferc, levels = c(0, 1), labels = c("random", "prefer"))
#### Create 1x2 plot for joint plot
par(mfrow = c(1, 2))
#### Random ----
# Calibrate
cf_random <- calibrate_counterfactual(d %>% filter(preferc == "random"),
                               statistic = "z_stat",
                               threshold = calibration_threshold)
# Plot
plot_counterfactual(d %>% filter(preferc == "random"),
                    statistic = "z_stat",
                    calibration = cf_random,
                    breaks = c(-10000, seq(-10, -2.75, 0.15), c(-4:-1*(2.576 - 1.96) / 4 - 1.96), c(-2:0 * 0.1575 - 1.645), seq(-1.5, 0, 0.15), seq(0, 10000, 0.15)),
                    # breaks = c(seq(0, 1.5, 0.15), c(0:2 * 0.1575 + 1.645), c(1:4*(2.576 - 1.96) / 4 + 1.96), 2.576, seq(2.75, 10, 0.15), 10000),
                    # breaks = c(seq(0, 10, 0.05), 10000),
                    # breaks = c(seq(0, 10, length.out = 50), 10000),
                    significance = sig_thresholds,
                    ylim = y_lims,
                    xlims = c(-10, 10),
                    show_params = FALSE,
                    star_spacing = 0.0075,
                    star_buffer = 0,
                    star_cex = 1)
# Add vertical zero line
abline(v = 0, col = "black", lty = 2)
#### Prefer ----
# Calibrate
cf_prefer <- calibrate_counterfactual(d %>% filter(preferc == "prefer"),
                               statistic = "z_stat",
                               threshold = calibration_threshold)
# Plot
plot_counterfactual(d %>% filter(preferc == "prefer"),
                    statistic = "z_stat",
                    calibration = cf_prefer,
                    breaks = c(-10000, seq(-10, -2.75, 0.15), c(-4:-1*(2.576 - 1.96) / 4 - 1.96), c(-2:0 * 0.1575 - 1.645), seq(-1.5, 0, 0.15), seq(0, 10000, 0.15)),
                    # breaks = c(seq(0, 1.5, 0.15), c(0:2 * 0.1575 + 1.645), c(1:4*(2.576 - 1.96) / 4 + 1.96), 2.576, seq(2.75, 10, 0.15), 10000),
                    # breaks = c(seq(0, 10, 0.05), 10000),
                    # breaks = c(seq(0, 10, length.out = 50), 10000),
                    significance = sig_thresholds,
                    ylim = y_lims,
                    xlims = c(-10, 10),
                    show_params = FALSE,
                    add_legend = FALSE,
                    star_spacing = 0.0075,
                    star_buffer = 0,
                    star_cex = 1)
# Add vertical zero line
abline(v = 0, col = "black", lty = 2)
# Add title
title("Random (left) vs Prefer (right)", line = -1, outer = TRUE)
## Save as PDF
dev.copy(pdf, paste0(figure_path, "z_stat_densities/", "figure_z_density_and_counterfactual_author_judgment.pdf"), 
         width = 10, 
         height = 6)
dev.off()

### OECD vs non OECD vs mixed OECD ----
d$OECDc <- factor(d$OECDc, levels = c(1, 2, 3), labels = c("OECD", "non OECD", "mixed OECD"))
#### Create 1x3 plot for joint plot
par(mfrow = c(1, 3))
#### OECD ----
# Calibrate
cf_OECD <- calibrate_counterfactual(d %>% filter(OECDc == "OECD"),
                               statistic = "z_stat",
                               threshold = calibration_threshold)
# Plot
plot_counterfactual(d %>% filter(OECDc == "OECD"),
                    statistic = "z_stat",
                    calibration = cf_OECD,
                    breaks = c(-10000, seq(-10, -2.75, 0.15), c(-4:-1*(2.576 - 1.96) / 4 - 1.96), c(-2:0 * 0.1575 - 1.645), seq(-1.5, 0, 0.15), seq(0, 10000, 0.15)),
                    # breaks = c(seq(0, 1.5, 0.15), c(0:2 * 0.1575 + 1.645), c(1:4*(2.576 - 1.96) / 4 + 1.96), 2.576, seq(2.75, 10, 0.15), 10000),
                    # breaks = c(seq(0, 10, 0.05), 10000),
                    # breaks = c(seq(0, 10, length.out = 50), 10000),
                    significance = sig_thresholds,
                    ylim = y_lims,
                    xlims = c(-10, 10),
                    show_params = FALSE,
                    star_spacing = 0.0075,
                    star_buffer = 0,
                    star_cex = 1)
# Add vertical zero line
abline(v = 0, col = "black", lty = 2)
#### non OECD ----
# Calibrate
cf_non_OECD <- calibrate_counterfactual(d %>% filter(OECDc == "non OECD"),
                               statistic = "z_stat",
                               threshold = calibration_threshold)
# Plot
plot_counterfactual(d %>% filter(OECDc == "non OECD"),
                    statistic = "z_stat",
                    calibration = cf_non_OECD,
                    breaks = c(-10000, seq(-10, -2.75, 0.15), c(-4:-1*(2.576 - 1.96) / 4 - 1.96), c(-2:0 * 0.1575 - 1.645), seq(-1.5, 0, 0.15), seq(0, 10000, 0.15)),
                    # breaks = c(seq(0, 1.5, 0.15), c(0:2 * 0.1575 + 1.645), c(1:4*(2.576 - 1.96) / 4 + 1.96), 2.576, seq(2.75, 10, 0.15), 10000),
                    # breaks = c(seq(0, 10, 0.05), 10000),
                    # breaks = c(seq(0, 10, length.out = 50), 10000),
                    significance = sig_thresholds,
                    ylim = y_lims,
                    xlims = c(-10, 10),
                    show_params = FALSE,
                    add_legend = FALSE,
                    star_spacing = 0.0075,
                    star_buffer = 0,
                    star_cex = 1)
# Add vertical zero line
abline(v = 0, col = "black", lty = 2)
#### mixed OECD ----
# Calibrate
cf_mixed_OECD <- calibrate_counterfactual(d %>% filter(OECDc == "mixed OECD"),
                               statistic = "z_stat",
                               threshold = calibration_threshold)
# Plot
plot_counterfactual(d %>% filter(OECDc == "mixed OECD"),
                    statistic = "z_stat",
                    calibration = cf_mixed_OECD,
                    breaks = c(-10000, seq(-10, -2.75, 0.15), c(-4:-1*(2.576 - 1.96) / 4 - 1.96), c(-2:0 * 0.1575 - 1.645), seq(-1.5, 0, 0.15), seq(0, 10000, 0.15)),
                    # breaks = c(seq(0, 1.5, 0.15), c(0:2 * 0.1575 + 1.645), c(1:4*(2.576 - 1.96) / 4 + 1.96), 2.576, seq(2.75, 10, 0.15), 10000),
                    # breaks = c(seq(0, 10, 0.05), 10000),
                    # breaks = c(seq(0, 10, length.out = 50), 10000),
                    significance = sig_thresholds,
                    ylim = y_lims,
                    xlims = c(-10, 10),
                    show_params = FALSE,
                    add_legend = FALSE,
                    star_spacing = 0.0075,
                    star_buffer = 0,
                    star_cex = 1)
# Add vertical zero line
abline(v = 0, col = "black", lty = 2)
# Add title
title("OECD (left) vs non-OECD (middle) vs mixed OECD (right)", line = -1, outer = TRUE)
## Save as PDF
dev.copy(pdf, paste0(figure_path, "z_stat_densities/", "figure_z_density_and_counterfactual_OECD_non_OECD_mixed_OECD.pdf"), 
         width = 10, 
         height = 6)
dev.off()


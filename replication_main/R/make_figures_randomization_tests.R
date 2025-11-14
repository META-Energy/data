# Setup ----

## Load required libraries ----
library(haven)
library(tidyverse)
library(JWileymisc)
library(patchwork)

## Load required functions ----
source(
  "replication_main/R/functions/run_randomization_test.R"
)
source(
  "replication_main/R/functions/randomization_test_battery.R"
)
source(
  "replication_main/R/functions/format_randomization_table.R"
)
source(
  "replication_main/R/functions/plot_randomization_tests.R"
)

## Load data ----
d_path <- "replication_main/metaenergy_final.dta" 
d <- haven::read_dta(d_path)

## Path for figures ----
figure_path <- "replication_main/R/figures/"

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

# Define windows ----
deltas <- seq(0.5, 0.05, by = -0.005)

# Define ylims ----
ylims <- c(0, 1)

# Data prep ----
d_neg_z <- d %>%
  # Calculate z-statistic
  mutate(z_stat = abs(coeffc / sec)) %>%
  # Filter for negative effects
  filter(coeffc <= 0)

# Full sample ----
### Test at 90 % level ----
random_test_90_all <- randomization_test_battery(
  data = d_neg_z,
  statistic = "z_stat",
  thresholds = c(1.645), # 90 % level
  group = NULL,
  deltas = deltas
)
#### Format and save table
table_random_test_90_all <- format_randomization_table(
  random_test_90_all,
  group_comparison = NULL,
  group_order = NULL
)
#### Visualize
(figure_random_test_90_all <- plot_randomization_tests(
  table_random_test_90_all,
  group_order = NULL,
  threshold = "1.645 (90 % level) - all",
  y_limits = ylims,
  show_n = TRUE,
  plot_type = "combined",
  n_plot_ratio = 0.3
))
#### Save figure as pdf
ggsave(
  paste0(figure_path, "randomization_tests/", "figure_random_test_90_all.pdf"),
  figure_random_test_90_all,
  width = 6.5,
  height = 5
)

### Test at 95 % level ----
random_test_95_all <- randomization_test_battery(
  data = d_neg_z,
  statistic = "z_stat",
  thresholds = c(1.96), # 95 % level
  group = NULL,
  deltas = deltas
)
#### Format and save table
table_random_test_95_all <- format_randomization_table(
  random_test_95_all,
  group_comparison = NULL,
  group_order = NULL
)
#### Visualize
(figure_random_test_95_all <- plot_randomization_tests(
  table_random_test_95_all,
  group_order = NULL,
  threshold = "1.96 (95 % level) - all",
  y_limits = ylims,
  show_n = TRUE,
  plot_type = "combined",
  n_plot_ratio = 0.3
))
#### Save figure as pdf
ggsave(
  paste0(figure_path, "randomization_tests/", "figure_random_test_95_all.pdf"),
  figure_random_test_95_all,
  width = 6.5,
  height = 5
)

### Test at 99 % level ----
random_test_99_all <- randomization_test_battery(
  data = d_neg_z,
  statistic = "z_stat",
  thresholds = c(2.576), # 99 % level
  group = NULL,
  deltas = deltas
)
#### Format and save table
table_random_test_99_all <- format_randomization_table(
  random_test_99_all,
  group_comparison = NULL,
  group_order = NULL
)
#### Visualize
(figure_random_test_99_all <- plot_randomization_tests(
  table_random_test_99_all,
  group_order = NULL,
  threshold = "2.576 (99 % level) - all",
  y_limits = ylims,
  show_n = TRUE,
  plot_type = "combined",
  n_plot_ratio = 0.3
))
#### Save figure as pdf
ggsave(
  paste0(figure_path, "randomization_tests/", "figure_random_test_99_all.pdf"),
  figure_random_test_99_all,
  width = 6.5,
  height = 5
)

# For short run ----
### Test at 90 % level ----
random_test_90_sr <- randomization_test_battery(
  data = d_neg_z %>% filter(horizond == 0),
  statistic = "z_stat",
  thresholds = c(1.645), # 90 % level
  group = NULL,
  deltas = deltas
)
#### Format and save table
table_random_test_90_sr <- format_randomization_table(
  random_test_90_sr,
  group_comparison = NULL,
  group_order = NULL
)
#### Visualize
(figure_random_test_90_sr <- plot_randomization_tests(
  table_random_test_90_sr,
  group_order = NULL,
  threshold = "1.645 (90 % level) - short run",
  y_limits = ylims,
  show_n = TRUE,
  plot_type = "combined",
  n_plot_ratio = 0.3
))
#### Save figure as pdf
ggsave(
  paste0(figure_path, "randomization_tests/", "figure_random_test_90_sr.pdf"),
  figure_random_test_90_sr,
  width = 6.5,
  height = 5
)

### Test at 95 % level ----
random_test_95_sr <- randomization_test_battery(
  data = d_neg_z %>% filter(horizond == 0),
  statistic = "z_stat",
  thresholds = c(1.96), # 95 % level
  group = NULL,
  deltas = deltas
)
#### Format and save table
table_random_test_95_sr <- format_randomization_table(
  random_test_95_sr,
  group_comparison = NULL,
  group_order = NULL
)
#### Visualize
(figure_random_test_95_sr <- plot_randomization_tests(
  table_random_test_95_sr,
  group_order = NULL,
  threshold = "1.96 (95 % level) - short run",
  y_limits = ylims,
  show_n = TRUE,
  plot_type = "combined",
  n_plot_ratio = 0.3
))
#### Save figure as pdf
ggsave(
  paste0(figure_path, "randomization_tests/", "figure_random_test_95_sr.pdf"),
  figure_random_test_95_sr,
  width = 6.5,
  height = 5
)

### Test at 99 % level ----
random_test_99_sr <- randomization_test_battery(
  data = d_neg_z %>% filter(horizond == 0),
  statistic = "z_stat",
  thresholds = c(2.576), # 99 % level
  group = NULL,
  deltas = deltas
)
#### Format and save table
table_random_test_99_sr <- format_randomization_table(
  random_test_99_sr,
  group_comparison = NULL,
  group_order = NULL
)
#### Visualize
(figure_random_test_99_sr <- plot_randomization_tests(
  table_random_test_99_sr,
  group_order = NULL,
  threshold = "2.576 (99 % level) - short run",
  y_limits = ylims,
  show_n = TRUE,
  plot_type = "combined",
  n_plot_ratio = 0.3
))
#### Save figure as pdf
ggsave(
  paste0(figure_path, "randomization_tests/", "figure_random_test_99_sr.pdf"),
  figure_random_test_99_sr,
  width = 6.5,
  height = 5
)

# For long run ----
### Test at 90 % level ----
random_test_90_lr <- randomization_test_battery(
  data = d_neg_z %>% filter(horizond == 1),
  statistic = "z_stat",
  thresholds = c(1.645), # 90 % level
  group = NULL,
  deltas = deltas
)
#### Format and save table
table_random_test_90_lr <- format_randomization_table(
  random_test_90_lr,
  group_comparison = NULL,
  group_order = NULL
)
#### Visualize
(figure_random_test_90_lr <- plot_randomization_tests(
  table_random_test_90_lr,
  group_order = NULL,
  threshold = "1.645 (90 % level) - long run",
  y_limits = ylims,
  show_n = TRUE,
  plot_type = "combined",
  n_plot_ratio = 0.3
))
#### Save figure as pdf
ggsave(
  paste0(figure_path, "randomization_tests/", "figure_random_test_90_lr.pdf"),
  figure_random_test_90_lr,
  width = 6.5,
  height = 5
)

### Test at 95 % level ----
random_test_95_lr <- randomization_test_battery(
  data = d_neg_z %>% filter(horizond == 1),
  statistic = "z_stat",
  thresholds = c(1.96), # 95 % level
  group = NULL,
  deltas = deltas
)
#### Format and save table
table_random_test_95_lr <- format_randomization_table(
  random_test_95_lr,
  group_comparison = NULL,
  group_order = NULL
)
#### Visualize
(figure_random_test_95_lr <- plot_randomization_tests(
  table_random_test_95_lr,
  group_order = NULL,
  threshold = "1.96 (95 % level) - long run",
  y_limits = ylims,
  show_n = TRUE,
  plot_type = "combined",
  n_plot_ratio = 0.3
))
#### Save figure as pdf
ggsave(
  paste0(figure_path, "randomization_tests/", "figure_random_test_95_lr.pdf"),
  figure_random_test_95_lr,
  width = 6.5,
  height = 5
)

### Test at 99 % level ----
random_test_99_lr <- randomization_test_battery(
  data = d_neg_z %>% filter(horizond == 1),
  statistic = "z_stat",
  thresholds = c(2.576), # 99 % level
  group = NULL,
  deltas = deltas
)
#### Format and save table
table_random_test_99_lr <- format_randomization_table(
  random_test_99_lr,
  group_comparison = NULL,
  group_order = NULL
)
#### Visualize
(figure_random_test_99_lr <- plot_randomization_tests(
  table_random_test_99_lr,
  group_order = NULL,
  threshold = "2.576 (99 % level) - long run",
  y_limits = ylims,
  show_n = TRUE,
  plot_type = "combined",
  n_plot_ratio = 0.3
))
#### Save figure as pdf
ggsave(
  paste0(figure_path, "randomization_tests/", "figure_random_test_99_lr.pdf"),
  figure_random_test_99_lr,
  width = 6.5,
  height = 5
)

# For top journal vs other ----
### Test at 90 % level ----
random_test_90_top <- randomization_test_battery(
  data = d_neg_z %>% filter(topjourd == 1),
  statistic = "z_stat",
  thresholds = c(1.645), # 90 % level
  group = NULL,
  deltas = deltas
)
#### Format and save table
table_random_test_90_top <- format_randomization_table(
  random_test_90_top,
  group_comparison = NULL,
  group_order = NULL
)
#### Visualize
(figure_random_test_90_top <- plot_randomization_tests(
  table_random_test_90_top,
  group_order = NULL,
  threshold = "1.645 (90 % level) - top journal",
  y_limits = ylims,
  show_n = TRUE,
  plot_type = "combined",
  n_plot_ratio = 0.3
))
#### Save figure as pdf
ggsave(
  paste0(figure_path, "randomization_tests/", "figure_random_test_90_top.pdf"),
  figure_random_test_90_top,
  width = 6.5,
  height = 5
)

### Test at 95 % level ----
random_test_95_top <- randomization_test_battery(
  data = d_neg_z %>% filter(topjourd == 1),
  statistic = "z_stat",
  thresholds = c(1.96), # 95 % level
  group = NULL,
  deltas = deltas
)
#### Format and save table
table_random_test_95_top <- format_randomization_table(
  random_test_95_top,
  group_comparison = NULL,
  group_order = NULL
)
#### Visualize
(figure_random_test_95_top <- plot_randomization_tests(
  table_random_test_95_top,
  group_order = NULL,
  threshold = "1.96 (95 % level) - top journal",
  y_limits = ylims,
  show_n = TRUE,
  plot_type = "combined",
  n_plot_ratio = 0.3
))
#### Save figure as pdf
ggsave(
  paste0(figure_path, "randomization_tests/", "figure_random_test_95_top.pdf"),
  figure_random_test_95_top,
  width = 6.5,
  height = 5
)

### Test at 99 % level ----
random_test_99_top <- randomization_test_battery(
  data = d_neg_z %>% filter(topjourd == 1),
  statistic = "z_stat",
  thresholds = c(2.576), # 99 % level
  group = NULL,
  deltas = deltas
)
#### Format and save table
table_random_test_99_top <- format_randomization_table(
  random_test_99_top,
  group_comparison = NULL,
  group_order = NULL
)
#### Visualize
(figure_random_test_99_top <- plot_randomization_tests(
  table_random_test_99_top,
  group_order = NULL,
  threshold = "2.576 (99 % level) - top journal",
  y_limits = ylims,
  show_n = TRUE,
  plot_type = "combined",
  n_plot_ratio = 0.3
))
#### Save figure as pdf
ggsave(
  paste0(figure_path, "randomization_tests/", "figure_random_test_99_top.pdf"),
  figure_random_test_99_top,
  width = 6.5,
  height = 5
)

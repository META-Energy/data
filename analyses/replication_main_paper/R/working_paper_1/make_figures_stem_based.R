# Setup ----

## Load required libraries ----
library(haven)
library(tidyverse)
library(JWileymisc)

# Load required functions
source("~/GitHub/MORPEP/META_CMP/data/analysis/R/stem_method.R")

## Load data ----
d_path <- "~/Nextcloud/project-vwl4makro/09_Forschung/Drittmittel/DZ2022/WP2_Meta_energy/estimation/metaenergy_final.dta" 
d <- haven::read_dta(d_path)

## Path for figures ----
figure_path <- "~/Nextcloud/project-vwl4makro/09_Forschung/Drittmittel/DZ2022/WP2_Meta_energy/estimation/R Franz/working_paper_1/figures/"

## Winsorization ----
wins_lev <- 0.02 # Set winsorization level
d <- d %>%
  mutate(sec = JWileymisc::winsorizor(sec, wins_lev, na.rm = TRUE)) %>%
  mutate(coeffc = JWileymisc::winsorizor(coeffc, wins_lev, na.rm = TRUE))

# Placeholder for results ----
stem_results <- list()

# For short run ----
d_stem_sr <- d %>% 
  filter(horizond == 1) %>% 
  select(coeffc, sec)
stem_results[["sr"]] <- stem(d_stem_sr$coeffc, 
                             d_stem_sr$sec,
                             param)
beepr::beep()
# Store the estimates in a table 
View(stem_results$sr$estimates)
# Plot the stem based funnel
(stem_funnel_sr <- stem_funnel(d_stem_sr %>% pull(coeffc), 
                              d_stem_sr %>% pull(sec), 
                              stem_results$sr$estimates,
                              beta_axis_min = -2,
                              beta_axis_max = 0.5))
# Save as PDF
dev.copy(pdf, paste0(figure_path, "stem_based/", "figure_stem_funnel_sr.pdf"), 
         width = 10, 
         height = 10)
dev.off()

# For long run ----
d_stem_lr <- d %>% 
  filter(horizond == 2) %>% 
  select(coeffc, sec)
stem_results[["lr"]] <- stem(d_stem_lr$coeffc, 
                             d_stem_lr$sec,
                             param)
stem_results$lr$estimates
beepr::beep()
# Store the estimates in a table 
View(stem_results$lr$estimates)
# Plot the stem based funnel
(stem_funnel_lr <- stem_funnel(d_stem_lr %>% pull(coeffc), 
                              d_stem_lr %>% pull(sec), 
                              stem_results$lr$estimates,
                              beta_axis_min = -2,
                              beta_axis_max = 0.5))
# Save as PDF
dev.copy(pdf, paste0(figure_path, "stem_based/", "figure_stem_funnel_lr.pdf"), 
         width = 10, 
         height = 10)
dev.off()

# Print and store estimation info in one table ----
# Add horizon to estimates
stem_estimates_sr <- as.data.frame(stem_results$sr$estimates)
stem_estimates_sr$horizon <- "short run"
stem_estimates_lr <- as.data.frame(stem_results$lr$estimates)
stem_estimates_lr$horizon <- "long run"
stem_estimates <- rbind(stem_estimates_sr, stem_estimates_lr)
# Save as RData
save(stem_estimates, file = paste0(figure_path, "stem_based/", "stem_estimates.RData"))
# Print as latex table, but without "multiple", "n_iteration" and "sd of total heterogeneity" columns, and rename n_stem as n studies
print(xtable::xtable(stem_estimates %>% 
                       select(-multiple, -n_iteration, -"sd of total heterogeneity") %>% 
                       rename("n studies in stem" = n_stem),
                     caption = "Stem-based estimation", digits = 4), type = "latex")
# Save as CSV
write.csv(stem_estimates, paste0(figure_path, "stem_based/", "table_stem_estimates.csv"), row.names = FALSE)


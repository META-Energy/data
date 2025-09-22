# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)

# Create the data frame from the Excel data (pblinsub_graphs.xlsx)
data <- data.frame(
  metric = c("UA_elec", "PEESE_elec", "UA_ngas", "PEESE_ngas", 
             "UA_OECD", "PEESE_OECD", "UA_nonOECD", "PEESE_nonOECD",
             "UA_res", "PEESE_res", "UA_bus", "PEESE_bus",
             "UA_ptop", "PEESE_ptop", "UA_pran", "PEESE_pran"),
  short_run = c(-0.308, -0.134, -0.19, -0.0507, 
                -0.249, -0.0897, -0.403, -0.322,
                -0.277, -0.209, -0.336, -0.0734,
                -0.381, -0.138, -0.256, -0.148),
  long_run = c(-0.471, -0.199, -0.398, -0.12, 
               -0.442, -0.211, -0.447, -0.332,
               -0.479, -0.297, -0.42, -0.084,
               -0.54, -0.101, -0.409, -0.251)
)

# Restructure data for plotting
plot_data <- data %>%
  mutate(group = case_when(
    grepl("elec|ngas", metric) ~ "Plot1",
    grepl("OECD|nonOECD", metric) ~ "Plot2",
    grepl("res|bus", metric) ~ "Plot3",
    grepl("ptop|pran", metric) ~ "Plot4"
  )) %>%
  mutate(line_type = ifelse(grepl("UA_", metric), "UA", "PEESE"),
         category = gsub("UA_|PEESE_", "", metric))

# Convert to long format for ggplot
long_data <- plot_data %>%
  pivot_longer(cols = c(short_run, long_run), 
               names_to = "run_type", 
               values_to = "value") %>%
  mutate(run_type = factor(run_type, 
                           levels = c("short_run", "long_run"),
                           labels = c("Short run", "Long run")))

# Function to create a standardized plot
create_plot <- function(data_subset, title, y_min = -0.6, y_max = 0) {
  ggplot(data_subset, aes(x = run_type, y = value, 
                          group = interaction(line_type, category),
                          color = category,
                          linetype = line_type)) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    scale_y_continuous(limits = c(y_min, y_max), 
                       breaks = seq(y_min, 0, 0.1)) +
    scale_linetype_manual(values = c("UA" = "dotted", "PEESE" = "solid")) +
    labs(title = title, x = "", y = "") +
    theme_minimal() +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          plot.margin = unit(c(0.5, 0.5, 1, 0.5), "cm"),
          panel.grid.minor = element_blank(),
          axis.text = element_text(size = 14),     # Axis labels
          axis.title = element_text(size = 16),    # Axis titles
          legend.text = element_text(size = 14),   # Legend text
          plot.title = element_text(size = 18)     # Plot title
          ) +
    scale_x_discrete(expand = c(0.1, 0.1))
}

# Create each plot
plot1 <- create_plot(
  filter(long_data, group == "Plot1"),
  title = NULL
)
plot1

plot2 <- create_plot(
  filter(long_data, group == "Plot2"),
  title = NULL
)
plot2

plot3 <- create_plot(
  filter(long_data, group == "Plot3"),
  title = NULL
)
plot3

plot4 <- create_plot(
  filter(long_data, group == "Plot4"),
  title = NULL
) +
  scale_color_discrete(labels = function(x) gsub("^(p.+)$", "\\1", x)) +
  guides(color = guide_legend(order = 1), 
         linetype = guide_legend(order = 2))
plot4

# Arrange all plots in a 2x2 grid
# combined_plot <- grid.arrange(plot1, plot2, plot3, plot4, ncol = 2)

# Save the plots as PDF
figure_path <- "~/Nextcloud/project-vwl4makro/09_Forschung/Drittmittel/DZ2022/WP2_Meta_energy/estimation/R Franz/working_paper_1/figures/UA_PEESE_subsample_plots/"
ggsave(paste0(figure_path, "plot1_elec_ngas.pdf"), plot1, width = 6, height = 4, dpi = 300)
ggsave(paste0(figure_path, "plot2_oecd_nonoecd.pdf"), plot2, width = 6, height = 4, dpi = 300)
ggsave(paste0(figure_path, "plot3_res_bus.pdf"), plot3, width = 6, height = 4, dpi = 300)
ggsave(paste0(figure_path, "plot4_ptop_pran.pdf"), plot4, width = 6, height = 4, dpi = 300)


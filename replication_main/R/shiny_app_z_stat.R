library(shiny)
library(haven)
library(tidyverse)
library(JWileymisc)

# Source the plotting function
source(
  "/Users/franzprante/GitHub/MORPEP/META_CMP/data/analysis/R/plot_counterfactual.R"
)

# UI
ui <- fluidPage(
  titlePanel(
    "Manual calibration of non-central t-distribution for z-statistics"
  ),

  sidebarLayout(
    sidebarPanel(
      width = 3,

      # Horizon selector
      selectInput(
        "horizon",
        "Select Horizon:",
        choices = c("short run", "long run"),
        selected = "short run"
      ),

      # df slider
      sliderInput(
        "df",
        "Degrees of Freedom (df):",
        min = 0,
        max = 10,
        value = 2,
        step = 0.1
      ),

      # ncp slider
      sliderInput(
        "ncp",
        "Non-centrality Parameter (ncp):",
        min = -3.5,
        max = 3.5,
        value = -1.81,
        step = 0.01
      ),

      # Threshold input
      numericInput(
        "threshold",
        "Calibration Threshold:",
        value = -5,
        step = 1
      ),

      hr(),

      # Display empirical mass above threshold
      h4("Diagnostics:"),
      textOutput("empirical_mass"),
      textOutput("theoretical_mass"),
      textOutput("difference"),

      hr(),

      # Additional plot controls
      h4("Plot Settings:"),
      numericInput(
        "kernel_adjust",
        "Kernel Adjust:",
        value = 0.9,
        min = 0.1,
        max = 2,
        step = 0.01
      ),
      numericInput(
        "kernel_bw",
        "Kernel Bandwidth:",
        value = 0.3,
        min = 0.1,
        max = 2,
        step = 0.01
      )
    ),

    mainPanel(
      width = 9,
      plotOutput("distPlot", height = "600px")
    )
  )
)

# Server
server <- function(input, output, session) {
  # Load and prepare data (reactive so it only loads once)
  d <- reactive({
    d_path <- "~/Nextcloud/project-vwl4makro/09_Forschung/Drittmittel/DZ2022/WP2_Meta_energy/estimation/metaenergy_final.dta"
    d <- haven::read_dta(d_path)

    # Drop estimates with assumed precision
    d <- d %>%
      filter(
        !(is.na(se_orig) &
          is.na(t_orig) &
          p_val_calc %in% c(0.001, 0.01, 0.05, 0.1, 0.5))
      )

    # Winsorization
    wins_lev <- 0.02
    d <- d %>%
      mutate(sec = JWileymisc::winsorizor(sec, wins_lev, na.rm = TRUE)) %>%
      mutate(coeffc = JWileymisc::winsorizor(coeffc, wins_lev, na.rm = TRUE))

    # Calculate z-statistic
    d <- d %>% mutate(z_stat = coeffc / sec)

    # Add horizon factor
    d$horizond <- factor(
      d$horizond,
      levels = c(0, 1),
      labels = c("short run", "long run")
    )

    return(d)
  })

  # Filtered data based on horizon selection
  filtered_data <- reactive({
    d() %>% filter(horizond == input$horizon)
  })

  # Calculate diagnostics
  diagnostics <- reactive({
    data <- filtered_data()
    threshold <- input$threshold # Changed from hardcoded -5

    # Empirical mass below/above threshold (depends on sign)
    if (threshold >= 0) {
      n_beyond <- sum(data$z_stat > threshold, na.rm = TRUE)
    } else {
      n_beyond <- sum(data$z_stat < threshold, na.rm = TRUE)
    }
    n_total <- sum(!is.na(data$z_stat))
    emp_mass <- n_beyond / n_total

    # Theoretical mass below/above threshold (using absolute values)
    abs_threshold <- abs(threshold)
    theo_mass <- 1 -
      (pt(abs_threshold, df = input$df, ncp = abs(input$ncp)) -
        pt(0, df = input$df, ncp = abs(input$ncp))) /
        (1 - pt(0, df = input$df, ncp = abs(input$ncp)))

    list(
      empirical = emp_mass,
      theoretical = theo_mass,
      difference = emp_mass - theo_mass,
      threshold = threshold # Store for display
    )
  })

  # Output diagnostics
  output$empirical_mass <- renderText({
    thresh <- diagnostics()$threshold
    direction <- ifelse(thresh >= 0, "above", "below")
    sprintf(
      "Empirical mass %s %.1f: %.4f",
      direction,
      thresh,
      diagnostics()$empirical
    )
  })

  output$theoretical_mass <- renderText({
    thresh <- diagnostics()$threshold
    direction <- ifelse(thresh >= 0, "above", "below")
    sprintf(
      "Theoretical mass %s %.1f: %.4f",
      direction,
      thresh,
      diagnostics()$theoretical
    )
  })

  output$difference <- renderText({
    diff <- diagnostics()$difference
    sprintf(
      "Difference: %.4f %s",
      abs(diff),
      ifelse(diff > 0, "(emp > theo)", "(emp < theo)")
    )
  })

  # Main plot
  output$distPlot <- renderPlot({
    # Create calibration object with user-selected parameters
    calibration <- list(
      df = input$df,
      ncp = input$ncp,
      fit_error = NA,
      n = nrow(filtered_data()),
      empirical_mass = diagnostics()$empirical
    )

    # Define thresholds
    sig_thresholds <- -c(1.645, 1.96, 2.576)
    y_lims <- c(0, 0.3)

    # Create plot
    plot_counterfactual(
      filtered_data(),
      statistic = "z_stat",
      calibration = calibration,
      breaks = c(
        -10000,
        seq(-10, -2.75, 0.15),
        c(-4:-1 * (2.576 - 1.96) / 4 - 1.96),
        c(-2:0 * 0.1575 - 1.645),
        seq(-1.5, 0, 0.15),
        seq(0, 10000, 0.15)
      ),
      kernel_adjust = input$kernel_adjust,
      kernel_bw = input$kernel_bw,
      significance = sig_thresholds,
      ylim = y_lims,
      xlims = c(-10, 10),
      show_params = TRUE,
      star_spacing = 0.0075,
      star_buffer = 0,
      star_cex = 1
    )

    # Add vertical zero line
    abline(v = 0, col = "black", lty = 2)

    # Add title with current parameters
    title(
      main = paste0(
        input$horizon,
        " (df=",
        input$df,
        ", ncp=",
        round(input$ncp, 2),
        ")"
      )
    )
  })
}

# Run the app
shinyApp(ui = ui, server = server)

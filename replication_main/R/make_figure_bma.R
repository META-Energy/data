# Bayesian model averaging 

## Load packages ---- 
library(haven)
library(dplyr)
library(fastDummies)
library(corrplot)
library(BMS)

## Load data ----
d_path <- "replication_main/metaenergy_final.dta" 
d <- haven::read_dta(d_path)

## Path for figures ----
figure_path <- "replication_main/R/figures/"

## Winsorization ----
wins_lev <- 0.02 # Set winsorization level
d <- d %>%
  mutate(sec = JWileymisc::winsorizor(sec, wins_lev, na.rm = TRUE)) %>%
  mutate(coeffc = JWileymisc::winsorizor(coeffc, wins_lev, na.rm = TRUE))

## Redefine variables as factors ---- 
d$horizond <- factor(d$horizond, levels = c(0, 1), labels = c("short run", "long run"))
d$energysourcem <- factor(d$energysourcem, levels = c(1, 2, 3), labels = c("elec", "natgas", "other"))
d$energyusec <- factor(d$energyusec, levels = c(3, 1, 2), labels = c("heatcoolmix", "heat", "cool"))
d$pricechanged <- factor(d$pricechanged, levels = c(1, 0), labels = c("market", "non-market, mix"))
d$OECDc <- factor(d$OECDc, levels = c(1, 2, 3), labels = c("OECD", "non OECD", "mixed OECD"))
d$sectorc <- factor(d$sectorc, levels = c(1, 2, 3), labels = c("resident", "business", "sector mix"))
d$macrod <- factor(d$macrod, levels = c(0, 1), labels = c("micro", "macro"))
d$datadimc <- factor(d$datadimc, levels = c(3, 1, 2), labels = c("panel", "crosssec", "timeseries"))
d$datafreqd <- factor(d$datafreqd, levels = c(1, 0), labels = c(">=Annual", "<Annual"))
d$estdynd <- factor(d$estdynd, levels = c(0, 1), labels = c("No", "Yes"))
d$estsysd <- factor(d$estsysd, levels = c(0, 1), labels = c("No", "Yes"))
d$identifd <- factor(d$identifd, levels = c(0, 1), labels = c("No", "Yes"))
d$inccontrd <- factor(d$inccontrd, levels = c(0, 1), labels = c("No", "Yes"))
d$xpricecontd <- factor(d$xpricecontd, levels = c(0, 1), labels = c("No", "Yes"))
d$topjourd <- factor(d$topjourd, levels = c(1, 0), labels = c("Yes", "No"))
d$byproduct <- factor(d$byproduct, levels = c(1, 0), labels = c("Yes", "No"))
d$preferc <- factor(d$preferc, levels = c(1, 0, 2), labels = c("random", "inferior", "prefer"))

# BMA ----

### Filter the data for the outcome variable and the variables for which BMA should be done ----
bma_data <- d %>% 
  mutate(
    SE2 = (sec)^2,
    logcit = log(ncitat + 1) # We add 1 to avoid the creation of -Inf values
  ) %>%
  select(
    
    # Baseline
    coeffc,
    ### P-bias test ###
    SE2,
    horizond,
    ### 
    energysourcem, 
    energyusec,
    ###
    pricechanged,
    OECDc, 
    sectorc,
    
    # Additional controls
    avyearcd,
    macrod, 
    datadimc, 
    datafreqd, 
    estdynd,
    estsysd,
    identifd,
    inccontrd,
    xpricecontd,
    topjourd, 
    logcit, 
    byproduct, 
    preferc
  )### Check if there are any character or factor columns and create dummies ----
char_factor_cols <- sapply(bma_data, function(x) is.character(x) || is.factor(x))
if (any(char_factor_cols)) {
  # Automatically create dummies for factor and character variables.
  # This sets as reference group the most frequent level of each factor variable
  # and removes the original factor variable to only use the newly created dummies. 
  bma_data <- fastDummies::dummy_cols(bma_data,
                                         remove_most_frequent_dummy = TRUE,
                                         remove_selected_columns = TRUE)
}
### Test if any NA values in bma_data and remove these ----
if (anyNA(bma_data)) {
  warning("Some of the selected variables contain NA values.
                Bayesian model averaging will ignore these observations.")
  bma_data <- bma_data[complete.cases(bma_data), ]
}
### BMS package requires a dataframe ----
bma_data <- as.data.frame(bma_data)

### Check cross-correlation ----
col<- colorRampPalette(c("red", "white", "blue"))
M <- round(cor(bma_data, use="complete.obs", method="pearson"), 2)
corrplot::corrplot (M, method="color", col=col(200), type="upper", addCoef.col = "black", number.cex=0.5, tl.pos = c("lt"), tl.col="black", tl.srt=45, sig.level = 0.01, insig = "blank")

### Estimation ----
bma_result <- BMS::bms(bma_data,
                          burn = 1e6,
                          iter = 3e6,
                          nmodel = 10000,
                          mprior = "dilut",
                          g = "UIP"
                       )

### Plot ----
bma_plot <- image(bma_result,
      yprop2pip = FALSE,
      cex.axis = 0.8)
bma_plot
### Save as PDF
dev.copy(pdf, paste0(figure_path, "bma/bma_plot.pdf"), 
         width = 9, 
         height = 10)
dev.off()

### Coefficient table ----
bma_table <- coef(bma_result, include.constant=T)
# Without last column "Idx"
bma_table <- bma_table[, -ncol(bma_table)]
# Round to 4 decimals
bma_table <- round(bma_table, 3)
bma_table
# Latex version
bma_table_latex <- xtable::xtable(bma_table, digits = 3)
xtable::print.xtable(bma_table_latex, type = "latex", file = paste0(figure_path, "bma/bma_table.tex"))

beepr::beep()


# 4_GAM_and_checks.R
# GAM tariff with a smooth age effect and a spatial smooth, model checks,
# Lorenz curves / Gini, and a safety loading (requires df from 0_setup_and_EDA.R).

library(dplyr)
library(tidyr)
library(knitr)
library(tibble)
library(mgcv)
library(forcats)

# All factor columns to relevel reference level
cat_vars <- c("coverage","fuel","sex","power","agecar","use","fleet","sport","split")

for (v in cat_vars) {
  # 1. Compute total exposure for each level
  expo_sum <- df %>%
    group_by(level = .data[[v]]) %>%
    summarise(total_expo = sum(expo, na.rm=TRUE), .groups="drop") %>%
    arrange(desc(total_expo))
  
  # 2. Identify the level with maximum exposure (as character)
  ref_lvl <- as.character(expo_sum$level[1])
  
  # 3. Move that level to the first position (reference level)
  df[[v]] <- fct_relevel(df[[v]], ref_lvl)
}

# Check that each factor has been re-leveled correctly
lapply(df[cat_vars], levels)


# —— 1. Fit GAM with spatial smoothing —— 
freq_gam <- gam(
  nclaims ~ 
    coverage + fuel + sex + power + agecar +
    use + fleet + sport +  split +        # factor variables
    s(ageph)+s(longitude, latitude) ,      # tensor smooth on coordinates
  offset = log(expo),
  data   = df,                            # the data frame
  family = poisson(link="log")
)
summary(freq_gam)

# Plot the smooth effect
# a darker blue line with a light-blue confidence ribbon
png("figures/gam_ageph_effect.png", width = 900, height = 650, res = 130)
plot(freq_gam,
     select     = 1,          # the ageph term
     shade      = TRUE,       # draw CI band
     se         = TRUE,       # show standard error ribbon
     shade.col  = "#99CCFF",  # ribbon fill
     col        = "#003366",  # smooth line color
     lwd        = 2,          # make the line a bit thicker
     rug        = FALSE       # turn off the rug
)
dev.off()

# Diagnostics for the smooth: check if k needs increasing or basis needs change
gam.check(freq_gam)

# Sequential ANOVA to test each term's significance
anova(freq_gam, test = "Chisq")
# Based on this analysis, 
# we removed use and sport, which are not significant
freq_gam2 <- update(
  freq_gam,
  . ~ . - use - sport
)
summary(freq_gam2)
AIC(freq_gam, freq_gam2)

gam.check(freq_gam2)
plot(freq_gam2, shade=TRUE)


# Try NB-GAM
freq_nb_gam <- gam(
  formula(freq_gam2),         # reuse the final Poisson-GAM formula
  family = nb(link="log"),    # NB family; theta estimated automatically
  data   = df,
  offset = lnexpo,
  method = "REML"             # REML (or GCV.Cp) recommended
)

# 2. View summary and estimated theta
summary(freq_nb_gam)

# 3. Compare AIC values
AIC(freq_gam2, freq_nb_gam)
#  NB-GAM's AIC is clearly lower, NB-GAM is superior

# 4. Check residual dispersion
cat("Poisson GAM dispersion:", deviance(freq_gam2) / df.residual(freq_gam2), "\n")
cat("NB–GAM dispersion:",     deviance(freq_nb_gam) / df.residual(freq_nb_gam),    "\n")


## Below is severity model analysis
# —— 1. Create subset with at least one claim —— 
sev_df <- df %>% 
  filter(nclaims > 0) %>% 
  # Add log_avg as the response variable
  mutate(log_avg = log(avg))

# Inspect the subset
glimpse(sev_df)



# —— 2. Fit the full Log-Normal GAM —— 
sev_gam_full <- gam(
  log_avg ~ coverage + fuel + sex + agecar + split + power + s(ageph),
  family  = gaussian(link = "identity"),
  weights = nclaims,
  data    = sev_df,
  method  = "REML"
)
summary(sev_gam_full)

# —— Drop-in-deviance test —— 
#    Conduct F-tests for each term in the full Log-Normal GAM
anova(sev_gam_full, test = "F")

# fuel, sex, power have p > 0.05, drop them

# —— Fit the simplified GAM —— 
sev_gam2 <- update(
  sev_gam_full,
  . ~ . - fuel - sex - power
)
summary(sev_gam2)

# Confirm simplification by AIC comparison
AIC(sev_gam_full, sev_gam2)

# —— Diagnostic plots: Residuals vs Fitted & Normal Q–Q —— 
png("figures/severity_diagnostics.png", width = 1300, height = 600, res = 130)
par(mfrow = c(1, 2), mar = c(4,4,2,1))

# 5.1 Residuals vs Fitted
resid2  <- resid(sev_gam2, type = "pearson")
fitted2 <- predict(sev_gam2, type = "response")
plot(
  fitted2, resid2,
  pch = 20, col = "#003366",
  xlab = "Fitted values", ylab = "Pearson residuals",
  main = "Residuals vs Fitted"
)
abline(h = 0, col = "red", lwd = 1)

# Normal Q–Q
qqnorm(
  resid2,
  pch = 20, col = "#003366",
  main = "Normal Q–Q"
)
qqline(resid2, col = "red", lwd = 1)

par(mfrow = c(1,1))
dev.off()


# Test model performance
library(dplyr)
library(tidyr)
library(knitr)
library(mgcv)

# —— 1. Re-split training/testing sets —— 
set.seed(2025)
n <- nrow(df)
train_idx <- sample(n, size = floor(0.8*n))
train_df  <- df[train_idx, ]
test_df   <- df[-train_idx, ]

# —— 2. Fit NB-GAM frequency model on training set —— 
# Tariff cells carry no location and no fleet flag, so the tariff frequency
# model keeps only the rating factors and the smooth age effect.
freq_nb_gam_train <- gam(
  nclaims ~ coverage + fuel + sex + power + agecar + split + s(ageph),
  family = nb(link="log"),
  data   = train_df,
  offset = log(expo),
  method = "REML"
)

# —— 3. Fit Log-Normal GAM severity model on training set —— 
sev_train <- train_df %>% 
  filter(nclaims > 0) %>% 
  mutate(log_avg = log(avg))

sev_gam_full_train <- gam(
  log_avg ~ coverage + fuel + sex + agecar + split + power + s(ageph),
  family  = gaussian(link="identity"),
  weights = nclaims,
  data    = sev_train,
  method  = "REML"
)

# Drop non-significant terms
anova(sev_gam_full_train, test="F")  
# fuel, sex, power have p > 0.05

sev_gam2_train <- update(
  sev_gam_full_train,
  . ~ . - fuel - sex - power
)

# Estimate bias-correction factor for log-normal
sigma2_train <- sum(resid(sev_gam2_train, type="pearson")^2) / df.residual(sev_gam2_train)


# —— 4. Make predictions on Full/Train/Test sets —— 
predict_all <- function(dat) {
  dat %>%
    mutate(
      pred_nclaims = predict(freq_nb_gam_train, newdata=., type="response"),
      pred_log_avg = predict(sev_gam2_train, newdata=., type="response"),
      pred_avg     = exp(pred_log_avg + sigma2_train/2),  # bias-corrected
      pred_paid    = pred_nclaims * pred_avg
    )
}

df_pred    <- predict_all(df)
train_pred <- predict_all(train_df)
test_pred  <- predict_all(test_df)


# —— 5. Summarize Actual vs Predicted —— 
cmp_tbl <- tibble(
  Dataset          = c("Full","Train","Test"),
  Actual_Claims    = c(sum(df_pred$nclaims), sum(train_pred$nclaims), sum(test_pred$nclaims)),
  Predicted_Claims = c(sum(df_pred$pred_nclaims), sum(train_pred$pred_nclaims), sum(test_pred$pred_nclaims)),
  Actual_Paid      = c(sum(df_pred$amount), sum(train_pred$amount), sum(test_pred$amount)),
  Predicted_Paid   = c(sum(df_pred$pred_paid), sum(train_pred$pred_paid), sum(test_pred$pred_paid))
)

kable(
  cmp_tbl,
  digits  = 0,
  caption = "Actual vs Predicted: Total Claims & Total Paid"
)


# —— 6. Construct Technical Tariff —— 
tariff_tbl <- crossing(
  coverage = levels(df$coverage),
  fuel     = levels(df$fuel),
  sex      = levels(df$sex),
  agecar   = levels(df$agecar),
  split    = levels(df$split),
  power    = levels(df$power)
) %>%
  # 1) Add required columns for prediction
  mutate(
    ageph = median(df$ageph, na.rm=TRUE),
    expo  = 1
  ) %>%
  # 2) Predict frequency & severity
  mutate(
    freq      = predict(freq_nb_gam_train, newdata = ., type = "response"),
    log_avg   = predict(sev_gam2_train,    newdata = ., type = "response"),
    avg       = exp(log_avg + sigma2_train/2),
    pure_prem = freq * avg
  ) %>%
  # 3) Select and rename final columns
  select(coverage, fuel, sex, agecar, split, power,
         freq, avg, pure_prem) %>%
  rename(
    `Freq (λ̂)`    = freq,
    `Sev (μ̂)`     = avg,
    `Pure Premium` = pure_prem
  )

knitr::kable(
  tariff_tbl,
  digits  = c(rep(0,6), 4, 2, 2),
  caption = "Technical Tariff Table\n(NB Frequency & Log‐Normal Severity)"
)

library(openxlsx)
write.xlsx(
  tariff_tbl,
  file      = "output/GAM_technical_tariff.xlsx",
  sheetName = "GAMTariff",
  rowNames  = FALSE
)


# Quality Checks

# install.packages("DescTools")
library(DescTools)

quality_check <- function(dat, title) {
  # 1. Compute Lorenz curve data manually
  df2 <- dat %>%
    arrange(desc(pred_paid)) %>%
    mutate(
      cum_prem = cumsum(pred_paid) / sum(pred_paid),
      cum_loss = cumsum(amount)     / sum(amount)
    )
  
  # 2. Plot Lorenz curve
  plot(
    df2$cum_prem, df2$cum_loss,
    type = "l", lwd = 2, col = "#003366",
    xlab = "Cumulative proportion of predicted pure premium",
    ylab = "Cumulative proportion of actual paid losses",
    main = paste0("Lorenz Curve — ", title)
  )
  abline(0, 1, col = "grey60", lty = 2)
  
  # 3. Compute Gini index (weighted)
  gini_val <- Gini(dat$amount, w = dat$pred_paid)
  
  # 4. Compute Loss Ratio
  loss_ratio <- sum(dat$amount,   na.rm = TRUE) / 
    sum(dat$pred_paid, na.rm = TRUE)
  
  # 5. Print results
  cat(sprintf(
    "%s: Gini = %.3f; Loss Ratio = %.3f\n",
    title, gini_val, loss_ratio
  ))
}

png("figures/lorenz_curves.png", width = 1600, height = 550, res = 130)
par(mfrow = c(1,3), mar = c(4,4,2,1))
quality_check(df_pred,    "Full")
quality_check(train_pred, "Train")
quality_check(test_pred,  "Test")
par(mfrow = c(1,1))
dev.off()


# Safety Loading

# Assume tariff_tbl already has column "Pure Premium"
pp <- tariff_tbl$`Pure Premium`

# 1. Compute mean and 95th percentile
mean_pp <- mean(pp, na.rm = TRUE)
q95_pp  <- quantile(pp, 0.95, na.rm = TRUE)

# 2. Compute safety loading
L <- as.numeric(q95_pp - mean_pp)

# 3. Add loading to tariff_tbl
tariff_tbl <- tariff_tbl %>%
  mutate(
    Safety_Loading = L,                     # constant load
    Loaded_Premium = `Pure Premium` + L      # loaded premium
  )

# 4. View the first 10 rows
kable(
  head(tariff_tbl, 10),
  digits = c(rep(0,6), 4, 2, 2, 2),
  col.names = c(
    "coverp","fuel","sex","agecar","split","power",
    "Freq","Sev","Pure Premium","Safety Loading","Loaded Premium"
  ),
  caption = "Example: First 10 Rating Cells with Safety Loading"
)

write.xlsx(
  tariff_tbl,
  file      = "output/GAM_technical_tariff_loaded.xlsx",
  sheetName = "Loaded Tariff",
  rowNames  = FALSE
)

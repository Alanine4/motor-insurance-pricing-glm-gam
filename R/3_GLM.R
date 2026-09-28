# 3_GLM.R
# GLM tariff: negative binomial frequency and log-normal severity, with
# covariate selection by deviance tests and AIC. Runs on its own.

# —— Environment setup ——
library(readr)
library(dplyr)
library(classInt)
library(ggplot2)
library(sf)
library(openxlsx)
library(tibble)
library(knitr)

# —— Read data and initial cleaning ——
df <- read_csv("data/Assignment.csv") %>% 
  rename_all(tolower) %>%          # convert all column names to lowercase
  mutate(
    # convert all categorical variables to factors
    sexp    = factor(sexp),
    fuelc   = factor(fuelc),
    split   = factor(split),
    usec    = factor(usec),
    fleetc  = factor(fleetc),
    sportc  = factor(sportc),
    coverp  = factor(coverp),
    powerc  = factor(powerc),
    agecar  = factor(agecar),
    # derive exposure and offset
    expo    = duree,      # the original 'duree' column is already annualized exposure
    lnexpo  = lnexpo,
    # average severity per claim (used later for severity modeling)
    sev_avg = ifelse(nbrtotc > 0, chargtot / nbrtotc, NA)
  ) %>%
  # filter out unrealistic ages
  filter(ageph >= 18, ageph <= 100)

# —— Split into training/testing sets ——
set.seed(2025)
train_idx <- sample(nrow(df), size = 0.8 * nrow(df))
train_df  <- df[train_idx, ]
test_df   <- df[-train_idx, ]

# —— Fit the "full model" Poisson GLM on the training set ——
library(MASS)   # for stepAIC

full_glm <- glm(
  nbrtotc ~ coverp + fuelc + sexp + agecar + split +
    usec + fleetc + sportc + powerc + ageph,
  family = poisson(link = "log"),
  offset = lnexpo,
  data   = train_df
)

# view summary
summary(full_glm)

# Selecting siginificant variables 
# build 10 candidate models (linear terms only, no smoothing)
g1  <- glm(nbrtotc ~ offset(lnexpo),
           family=poisson(link="log"),
           data=train_df)

g2  <- glm(nbrtotc ~ sexp + offset(lnexpo),
           family=poisson(link="log"),
           data=train_df)

g3  <- glm(nbrtotc ~ fuelc + offset(lnexpo),
           family=poisson(link="log"),
           data=train_df)

g4  <- glm(nbrtotc ~ coverp + offset(lnexpo),
           family=poisson(link="log"),
           data=train_df)

g5  <- glm(nbrtotc ~ powerc + offset(lnexpo),
           family=poisson(link="log"),
           data=train_df)

g6  <- glm(nbrtotc ~ sexp + fuelc + offset(lnexpo),
           family=poisson(link="log"),
           data=train_df)

g7  <- glm(nbrtotc ~ sexp + coverp + offset(lnexpo),
           family=poisson(link="log"),
           data=train_df)

g8  <- glm(nbrtotc ~ fuelc + coverp + offset(lnexpo),
           family=poisson(link="log"),
           data=train_df)

g9  <- glm(nbrtotc ~ sexp + fuelc + coverp + offset(lnexpo),
           family=poisson(link="log"),
           data=train_df)

g10 <- glm(nbrtotc ~ sexp*fuelc + coverp + offset(lnexpo),
           family=poisson(link="log"),
           data=train_df)



# compare models: effect of each covariate
anova(g1, g2, test="Chisq")  # effect of sexp
anova(g1, g3, test="Chisq")  # effect of fuelc
anova(g1, g4, test="Chisq")  # effect of coverp
anova(g1, g5, test="Chisq")  # effect of powerc

# sequential addition tests
anova(g2, g6, test="Chisq")  # g2→g6: add fuelc
anova(g2, g7, test="Chisq")  # g2→g7: add coverp
anova(g3, g6, test="Chisq")  # g3→g6: add sexp
anova(g3, g8, test="Chisq")  # g3→g8: add coverp
anova(g4, g7, test="Chisq")  # g4→g7: add sexp
anova(g4, g8, test="Chisq")  # g4→g8: add fuelc
anova(g6, g9, test="Chisq")  # g6(sexp+fuelc)→g9(+coverp)
anova(g7, g9, test="Chisq")  # g7(sexp+coverp)→g9(+fuelc)
anova(g8, g9, test="Chisq")  # g8(fuelc+coverp)→g9(+sexp)
anova(g9, g10, test="Chisq") # g9→g10: add sexp:fuelc interaction

# the results show that 
# all tested parameters are significant, except for the interaction.
# thus, we don't need the interaction effect.
# Below is another test for covariate selection

# assume the final order of covariates to test is:
vars <- c("coverp","fuelc","sexp","agecar","split",
          "usec","fleetc","sportc","powerc","ageph")

# 1) generate formulas step by step, for example:
#   g0: nbrtotc ~ 1
#   g1: nbrtotc ~ coverp
#   g2: nbrtotc ~ coverp + fuelc
#   ...
forms <- lapply(0:length(vars), function(k) {
  rhs <- if (k==0) "1" else paste(vars[1:k], collapse=" + ")
  as.formula(paste("nbrtotc ~", rhs))
})

# 2) fit each model with lapply
models <- lapply(forms, function(fm) {
  glm(fm,
      data = train_df,
      family = poisson(link="log"),
      offset = lnexpo)
})

# 3) chain them with anova() and perform Chi-squared tests
dev_table <- do.call(
  anova,
  c(models, list(test = "Chisq"))
)
print(dev_table)

# the results show that 
# usec is not significant and should be removed from the model
# Below is the updated model, from full_model
pois_mod <- glm(nbrtotc ~ coverp + fuelc + sexp + agecar +
                  split + fleetc + sportc + powerc + ageph,
                family = poisson(link="log"), offset = lnexpo, data = train_df)

# analysis using drop in deviance
drop1(pois_mod, test="Chisq")

# the results show that fleetc and sportc are not significant 
# should be removed from the model
# Below is the updated model 
pois_slim <- glm(nbrtotc ~ coverp + fuelc + sexp + agecar +
                   split + powerc + ageph,
                 family = poisson(link="log"), offset = lnexpo, data = train_df)

# compare AIC with the original model
AIC(pois_mod, pois_slim)  # indeed, AIC of pois_slim is lower than pois_mod
# pois_slim is the final model, using Poisson 
# looking for other ditribution that maybe better

# —— Fit Negative Binomial ——
library(MASS)
nb_slim <- glm.nb(
  nbrtotc ~ coverp + fuelc + sexp + agecar +
    split + powerc + ageph +
    offset(lnexpo),
  data = train_df
)

summary(pois_slim)
summary(nb_slim)

AIC(pois_slim, nb_slim)
# nb_mod has a much smaller AIC, meaning the NB fit is better


# —— Severity —— 
# —— calculate average severity per claim & fit Gamma model ——
# keep only records with at least one claim
sev_df <- df %>%
  filter(nbrtotc > 0) %>%
  mutate(sev_avg = chargtot / nbrtotc)

# fit Gamma regression (log-link), using claim counts as weights
sev_mod <- glm(
  sev_avg ~ coverp + fuelc + sexp + agecar + split +
    powerc + ageph,
  family  = Gamma(link = "log"),
  weights = nbrtotc,
  data    = sev_df
)
summary(sev_mod)

# using log normal 
# —— Log-Normal —— (identity link)
sev_ln <- glm(
  log(sev_avg) ~ coverp + fuelc + sexp + agecar + split +
    powerc + ageph,
  family  = gaussian(link = "identity"),
  weights = nbrtotc,
  data    = sev_df
)
summary(sev_ln)

# AIC 
AIC(sev_mod, sev_ln)
#sev_ln is better, because its AIC is lower

# drop-in-deviance 
drop1(sev_ln, test = "F")

# remove fuelc, sexp and powerc:
sev_ln_slim <- update(
  sev_ln,
  . ~ .- fuelc - sexp  - powerc
)
summary(sev_ln_slim)


AIC(sev_ln, sev_ln_slim)


# —— Construct Technical Tariff —— 
# sigma^2 = residual sum of squares / residual degrees of freedom
sigma2 <- sum(resid(sev_ln_slim, type="response")^2) / df.residual(sev_ln)


library(dplyr)
library(tidyr)
library(knitr)


grid_tariff <- crossing(
  coverp = levels(train_df$coverp),
  fuelc  = levels(train_df$fuelc),
  sexp   = levels(train_df$sexp),
  agecar = levels(train_df$agecar),
  split  = levels(train_df$split),
  powerc = levels(train_df$powerc)
) %>%
  mutate(
  
    ageph  = median(df$ageph, na.rm = TRUE),
    lnexpo = 0  # expo = 1
  )

# Frequency and Severity Prediction
grid_tariff <- grid_tariff %>%
  mutate(
    
    freq_pred   = predict(nb_slim,      newdata = ., type = "response"),
    
    log_sev_pred= predict(sev_ln_slim,   newdata = ., type = "response"),
    
    sev_pred_ln = exp(log_sev_pred + sigma2/2),
    
    pure_prem_ln= freq_pred * sev_pred_ln
  )



# Tariff 
tariff_tbl <- as.data.frame(grid_tariff)[ , c(
  "coverp","fuelc","sexp","agecar","split","powerc",
  "freq_pred","sev_pred_ln","pure_prem_ln"
)]

colnames(tariff_tbl) <- c(
  "Cover","Fuel","Sex","AgeCar","Split","Power",
  "Freq (λ̂)","Sev (μ̂) LN","Pure Premium"
)


library(knitr)
kable(
  tariff_tbl,
  digits = c(rep(0,6), 4, 0, 2),
  caption = "Technical Tariff Table\n(NB Frequency & Log‐Normal Severity)"
)

# Export "technical_tariff.xlsx"
write.xlsx(
  tariff_tbl,
  file      = "output/GLM_technical_tariff.xlsx",
  sheetName = "Tariff",
  rowNames  = FALSE
)


# -- Predict and comparison with actual data --

df  <- df  %>% 
  mutate(
    pred_nclaims = predict(nb_slim,      newdata = ., type = "response"),
    pred_log_sev = predict(sev_ln_slim,  newdata = ., type = "response"),
    pred_sev     = exp(pred_log_sev + sigma2/2),
    pred_paid    = pred_nclaims * pred_sev
  )

train_df <- train_df %>% 
  mutate(
    pred_nclaims = predict(nb_slim,      newdata = ., type = "response"),
    pred_log_sev = predict(sev_ln_slim,  newdata = ., type = "response"),
    pred_sev     = exp(pred_log_sev + sigma2/2),
    pred_paid    = pred_nclaims * pred_sev
  )

test_df  <- test_df %>% 
  mutate(
    pred_nclaims = predict(nb_slim,      newdata = ., type = "response"),
    pred_log_sev = predict(sev_ln_slim,  newdata = ., type = "response"),
    pred_sev     = exp(pred_log_sev + sigma2/2),
    pred_paid    = pred_nclaims * pred_sev
  )


cmp_tbl <- tibble(
  Dataset               = c("Full", "Train", "Test"),
  Actual_Total_Claims   = c(
    sum(df$nbrtotc,    na.rm=TRUE),
    sum(train_df$nbrtotc, na.rm=TRUE),
    sum(test_df$nbrtotc,  na.rm=TRUE)
  ),
  Predicted_Total_Claims= c(
    sum(df$pred_nclaims,    na.rm=TRUE),
    sum(train_df$pred_nclaims, na.rm=TRUE),
    sum(test_df$pred_nclaims,  na.rm=TRUE)
  ),
  Actual_Total_Paid     = c(
    sum(df$chargtot,    na.rm=TRUE),
    sum(train_df$chargtot, na.rm=TRUE),
    sum(test_df$chargtot,  na.rm=TRUE)
  ),
  Predicted_Total_Paid  = c(
    sum(df$pred_paid,    na.rm=TRUE),
    sum(train_df$pred_paid, na.rm=TRUE),
    sum(test_df$pred_paid,  na.rm=TRUE)
  )
)

# comparsion table
kable(
  cmp_tbl,
  digits = c(0,0,0,0,0),
  caption = "Actual vs Predicted: Total Claims and Total Paid"
)


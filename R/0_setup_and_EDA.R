# 0_setup_and_EDA.R
# Reads the policy data, joins postal-code coordinates, and produces the
# exploratory plots and portfolio summary statistics.
# Run from the repository root (see run_all.R).

## ----setup-------------------------------------------------------------------------
library(tidyverse)
library(sf)
library(mgcv)
library(gridExtra)
library(readxl)

KULbg <- "#116E8A"

# -- 1. Read main dataset
df <- read_csv("data/Assignment.csv") %>% 
  rename_all(tolower) %>% 
  # Ensure postal code is character
  mutate(codposs = as.character(codposs))

# -- 2. Read postal code reference table
insp <- read_excel("data/inspost.xls", sheet = 1) %>% 
  rename_all(tolower) %>% 
  # Ensure postal code is character
  mutate(codposs = as.character(codposs))

insp_sub <- insp %>% 
  # Select postal code and community, latitude, longitude columns
  select(
    codposs,
    community = commune,
    latitude  = lat,
    longitude = long
  ) %>%
  # Convert new columns to appropriate types
  mutate(
    community = as.factor(community),
    latitude  = as.numeric(latitude),
    longitude = as.numeric(longitude)
  )

# -- 3. Merge datasets
df <- df %>% 
  left_join(insp_sub, by = "codposs")

# At this point df has community, latitude, longitude appended
glimpse(df)

# -- 4. Rename and derive variables
df <- df %>% 
  rename_all(tolower) %>%
  rename(
    nclaims  = nbrtotc,
    expo     = duree,
    amount   = chargtot,
    agecar   = agecar,
    sex      = sexp,
    fuel     = fuelc,
    split    = split,
    use      = usec,
    fleet    = fleetc,
    sport    = sportc,
    coverage = coverp,
    power    = powerc
  ) %>%
  mutate(
    # Compute average claim severity per nonzero claim
    avg     = ifelse(nclaims > 0, amount / nclaims, NA),
    # Convert continuous variables to numeric
    nclaims = as.numeric(nclaims),
    expo    = as.numeric(expo),
    amount  = as.numeric(amount),
    avg     = as.numeric(avg),
    ageph   = as.numeric(ageph),
    # Convert categorical variables to factors
    agecar   = factor(agecar),
    sex      = factor(sex),
    fuel     = factor(fuel),
    split    = factor(split),
    use      = factor(use),
    fleet    = factor(fleet),
    sport    = factor(sport),
    coverage = factor(coverage),
    power    = factor(power)
  )

# -- 5. Set color scheme and define plotting functions
col   <- KULbg
fill  <- KULbg
ylab  <- "Relative frequency"

ggplot.bar <- function(data, var, xlab){
  ggplot(data, aes_string(x = var)) + theme_bw() +
    geom_bar(aes(y = (..count..)/sum(..count..)),
             col = col, fill = fill, alpha = 0.5) +
    labs(x = xlab, y = ylab)
}

ggplot.hist <- function(data, var, xlab, bw){
  # Filter out NA values for the variable
  dt <- data %>% filter(!is.na(.data[[var]]))
  ggplot(dt, aes_string(x = var)) + theme_bw() +
    geom_histogram(aes(y = (..count..)/sum(..count..)),
                   binwidth = bw,
                   col = col, fill = fill, alpha = 0.5) +
    labs(x = xlab, y = ylab)
}

# -- 6. Create EDA plots
plot.nclaims  <- ggplot.hist(df,  "nclaims",  "nclaims", 1)
plot.expo     <- ggplot.hist(df,  "expo",     "expo",    0.05)

df_sev <- df %>% filter(amount > 0, !is.na(avg))
plot.amount  <- ggplot(df_sev, aes(x = avg)) +
  theme_bw() +
  geom_density(adjust = 3, col = col, fill = fill, alpha = 0.5) +
  coord_cartesian(xlim = c(0, quantile(df_sev$avg, 0.99))) +
  labs(x = "severity", y = ylab)

plot.coverage <- ggplot.bar(df, "coverage", "coverage")
plot.fuel     <- ggplot.bar(df, "fuel",     "fuel")
plot.sex      <- ggplot.bar(df, "sex",      "sex")
plot.use      <- ggplot.bar(df, "use",      "use")
plot.fleet    <- ggplot.bar(df, "fleet",    "fleet")
plot.sport    <- ggplot.bar(df, "sport",    "sport")
plot.split    <- ggplot.bar(df, "split",    "split")

plot.ageph    <- ggplot.hist(df, "ageph",    "ageph",   2)
plot.agecar   <- ggplot.bar(df, "agecar",   "agecar")
plot.power    <- ggplot.bar(df, "power",    "power")

# -- 7. Arrange plots in a grid
eda_grid <- arrangeGrob(
  plot.nclaims, plot.expo,   plot.amount,   plot.coverage,
  plot.fuel,    plot.sex,    plot.use,      plot.fleet,
  plot.sport,   plot.split,  plot.ageph,    plot.agecar,
  plot.power,
  ncol = 4
)
grid::grid.draw(eda_grid)
ggsave("figures/eda_distributions.png", eda_grid, width = 12, height = 10, dpi = 150)

# 8. Calculate the ratio of
# the total number of claims and the total exposure in years and
# the ratio of the total claim amounts and the total number of claims
overall_summary <- df %>%
  summarise(
    total_claims   = sum(nclaims, na.rm = TRUE),
    total_exposure = sum(expo,    na.rm = TRUE),
    total_amount   = sum(amount,  na.rm = TRUE)
  ) %>%
  mutate(
    claim_frequency = total_claims / total_exposure,
    avg_severity    = total_amount   / total_claims
  )

print(overall_summary)


# 9. calculate the ratio exactly
cat("=== Categorical proportions ===\n")
vars <- c("agecar", "coverage", "fuel", "sex", "use", "fleet", "split", "sport", "power")

for (v in vars) {
  cat("\n--", toupper(v), "--\n")
  df %>%
    count(!!sym(v)) %>%
    mutate(proportion = n / sum(n)) %>%
    print()
}


# 10. share of policyholders between the 18th and 80th age percentiles
q <- quantile(df$ageph, probs = c(0.18, 0.8), na.rm = TRUE)
iqr_prop <- df %>% 
  summarise(prop = mean(ageph >= q[1] & ageph <= q[2], na.rm = TRUE))

print(iqr_prop)

# 11. quartiles of ageph (25%, 50%, 75%)
age_quantiles <- quantile(df$ageph, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
print(age_quantiles)


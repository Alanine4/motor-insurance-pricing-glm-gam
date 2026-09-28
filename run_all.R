# run_all.R
# Runs the full analysis from the repository root:
#   Rscript run_all.R
# Scripts 1, 2 and 4 use the data frame built in script 0; script 3 reads the
# data again on its own, so it runs last.

scripts <- c("R/0_setup_and_EDA.R",
             "R/1_spatial_EDA.R",
             "R/2_spatial_GAM.R",
             "R/4_GAM_and_checks.R",
             "R/3_GLM.R")

for (s in scripts) {
  message("==> ", s)
  source(s, echo = FALSE, print.eval = TRUE)
}

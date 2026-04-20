#!/usr/bin/env Rscript
# Export Model 10 coefficients as JSON for pure-Python prediction
# Run this monthly to refresh: Rscript export_model.R

library(tidyverse)
library(jsonlite)

cat("=== Loading data ===\n")
df <- read_csv('data/hdb_analysis.csv', show_col_types = FALSE)

# Derived variables (must match Notebook 6 exactly)
df$remaining_lease_sq <- df$remaining_lease_years^2
df$month_factor <- factor(format(df$month, '%Y-%m'))

cat(sprintf("Loaded %s rows\n", scales::comma(nrow(df))))
cat(sprintf("Date range: %s to %s\n", min(df$month), max(df$month)))

cat("\n=== Fitting Model 10 ===\n")
model10 <- lm(resale_price ~ town + flat_type + floor_area_sqm + storey_mid +
              remaining_lease_years + remaining_lease_sq +
              flat_model_grouped +
              dist_cbd_km + mrt_dist_m + hawker_dist_m +
              popular_school_dist_m +
              park_dist_m + hospital_dist_m +
              columbarium_dist_m + temple_dist_m +
              coast_dist_m +
              num_eights_tail +
              price_has_168 +
              block_has_4 +
              cny_month +
              month_factor,
              data = df)

s <- summary(model10)
cat(sprintf("R-squared: %.4f\n", s$r.squared))
cat(sprintf("Adj R-squared: %.4f\n", s$adj.r.squared))
cat(sprintf("Residual SE: %.0f\n", s$sigma))

# === Export coefficients ===
coefs <- coef(model10)

# Identify baseline (reference) levels for each categorical variable
# R drops the first alphabetical level by default
cat_vars <- list(
  town = sort(unique(df$town)),
  flat_type = sort(unique(df$flat_type)),
  flat_model_grouped = sort(unique(df$flat_model_grouped)),
  month_factor = sort(unique(as.character(df$month_factor)))
)

baselines <- lapply(cat_vars, function(x) x[1])

# Build the export object
export <- list(
  coefficients = as.list(coefs),
  baselines = baselines,
  factor_levels = cat_vars,
  metadata = list(
    r_squared = s$r.squared,
    adj_r_squared = s$adj.r.squared,
    residual_se = s$sigma,
    n_obs = nrow(df),
    date_range_start = as.character(min(df$month)),
    date_range_end = as.character(max(df$month)),
    exported_at = as.character(Sys.time())
  )
)

# Save
write_json(export, "data/model_coefficients.json", pretty = TRUE, auto_unbox = TRUE)
cat("\nExported to data/model_coefficients.json\n")

# Verify round-trip
cat("\n=== Verification ===\n")
cat(sprintf("Total coefficients: %d\n", length(coefs)))
cat(sprintf("Intercept: $%s\n", scales::comma(round(coefs["(Intercept)"]))))
cat(sprintf("Continuous vars: floor_area_sqm=$%s, storey_mid=$%s\n",
    scales::comma(round(coefs["floor_area_sqm"])),
    scales::comma(round(coefs["storey_mid"]))))
cat("Done.\n")

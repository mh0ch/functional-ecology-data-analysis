# =============================================================================
# Compute R10 per measurement using the within-plot Q10 fit
# =============================================================================
# Approach:
#   1. Load and clean the Excel data
#   2. Fit Q10 from within-plot variation only, by including plot_id as a
#      fixed effect — this isolates the temperature response from between-plot
#      biomass differences
#   3. Use the fitted Q10 to standardize every measurement to 10 °C
#   4. Save the result to a CSV for downstream analysis
# =============================================================================

# ---- 1. Load packages ------------------------------------------------------
library(tidyverse)
library(readxl)

# ---- 2. Read & clean -------------------------------------------------------
path   <- "data/NEE.xlsx"
header <- names(read_excel(path, sheet = "ER_GPP", n_max = 0))

d <- read_excel(path, sheet = "ER_GPP", skip = 2, col_names = header) |>
  rename(plot_id = Plot, day = Tag, temp = Bodentemp,
         par = PAR, nee = NEE, reco = ER, gpp = GPP) |>
  mutate(site = substr(plot_id, 1, 1),
         plot = substr(plot_id, 2, 2)) |>
  select(site, plot, plot_id, day, temp, par, reco, gpp)

glimpse(d)

# ---- 3. Within-plot Q10 fit ------------------------------------------------
# Take logs of the Q10 equation:
#     log(Reco) = log(R10) + ((T - 10)/10) * log(Q10)
# This is now linear in the parameters log(R10) and log(Q10), so we can fit
# it with lm(). By adding plot_id as a categorical predictor, each plot gets
# its own intercept (= its own log R10). The temperature slope is then
# estimated AFTER controlling for plot identity, so it reflects only the
# within-plot temperature response (the change between each plot's two
# measurement days), free of between-plot biomass differences.

fit_within <- lm(log(reco) ~ I((temp - 10)/10) + plot_id, data = d)

# Extract Q10: exponentiate the temperature slope to undo the log transform
Q10_within <- exp(coef(fit_within)["I((temp - 10)/10)"])

cat("\n--- Within-plot Q10 fit ---\n")
cat("Q10 =", round(Q10_within, 3), "\n\n")

# Show the fit summary (R^2, p-value on the temp slope, etc.)
print(summary(fit_within))

# ---- 4. Standardize every measurement to 10 °C -----------------------------
# Using the within-plot Q10, back out the temperature effect from each
# measurement:
#     R10_i = Reco_i / Q10^((T_i - 10)/10)

d <- d |>
  mutate(r10 = reco / Q10_within^((temp - 10)/10))

# ---- 5. Print the R10 values -----------------------------------------------
cat("\n--- R10 per measurement ---\n")
d |>
  select(site, plot, plot_id, day, temp, reco, r10) |>
  mutate(across(c(temp, reco, r10), \(x) round(x, 3))) |>
  print(n = Inf)

# ---- 6. Save to CSV --------------------------------------------------------
dir.create("data", showWarnings = FALSE)
write_csv(d, "data/r10_per_measurement.csv")

cat("\nSaved: data/r10_per_measurement.csv\n")
cat("Q10 used for standardization:", round(Q10_within, 3), "\n")

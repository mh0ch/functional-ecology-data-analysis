# ---- 1. Load packages 
library(tidyverse)   # dplyr for data manipulation, ggplot2 for plotting
library(readxl)      # read Excel files directly

# ---- 2. Read the data 
# Row 1: column headers (Plot, Tag, Bodentemp, PAR, NEE, ...)
# Row 2: units (°C, µmol m-2 s-2, ...) — we skip this
# Row 3 onwards: actual data
#
# Trick: read the header row once to get the column names, then re-read the
# file skipping both the header AND the units row, applying the header manually.

path <- "data/NEE.xlsx"   # adjust if your file is elsewhere

header <- names(read_excel(path, sheet = "ER_GPP", n_max = 0))      # just grab the column names

d_raw <- read_excel(
  path,
  sheet = "ER_GPP",
  skip      = 2, # skip header + units row
  col_names = header
)

# Quick look to confirm it loaded correctly
glimpse(d_raw)

# ---- 3. Clean and rename 
# - Rename columns to be R-friendly (no spaces, no special chars)
# - Split the "Plot" column (e.g. "A1") into "site" (A) and "plot" (1)
# - Keep only the columns we need

d <- d_raw |>
  rename(
    plot_id = Plot,
    day     = Tag,
    temp    = Bodentemp,
    par     = PAR,
    nee     = NEE,
    reco    = ER,
    gpp     = GPP
  ) |>
  mutate(
    site = substr(plot_id, 1, 1),          # first character (A/B/C/D)
    plot = substr(plot_id, 2, 2)           # second character (1/2/3)
  ) |>
  select(site, plot, plot_id, day, temp, par, reco, gpp)

# Confirm the structure looks right: site, plot, temp, reco columns visible
glimpse(d)

# Quick descriptive summary — important sanity checks:
# - temperature range (need some variation for Q10 to fit)
# - any weird Reco values
cat("\n--- Data summary ---\n")
cat("Number of measurements:", nrow(d), "\n")
cat("Temperature range (°C):", round(range(d$temp), 2), "\n")
cat("Reco range (µmol m⁻² s⁻¹):", round(range(d$reco), 2), "\n\n")

# ---- 4. Fit the global Q10 model 
# Model: Reco(T) = R10 * Q10^((T - 10) / 10)
#
# nls() = non-linear least squares. It finds the values of R10 and Q10 that
# minimize the sum of squared residuals between measured and predicted Reco.
# It's iterative and needs starting values — sensible guesses for ecosystem
# respiration are R10 ≈ 2 µmol m⁻² s⁻¹ and Q10 ≈ 2.

fit <- nls(
  reco ~ R10 * Q10^((temp - 10) / 10),
  data  = d,
  start = list(R10 = 2, Q10 = 2)
)

# Print the model summary: parameter estimates, standard errors, p-values
cat("--- Q10 model fit ---\n")
print(summary(fit))

# Extract the fitted parameters for use in the next step
R10_global <- coef(fit)["R10"]
Q10_global <- coef(fit)["Q10"]

cat("\nFitted R10 (global, at 10 °C):", round(R10_global, 3),
    "µmol m⁻² s⁻¹\n")
cat("Fitted Q10:", round(Q10_global, 3),
    "  (rate change per 10 °C warming)\n\n")

# ---- 5. Standardize every measurement to 10 °C 
# Rearranging the Q10 equation:
#   R10_i = Reco_i / Q10^((T_i - 10) / 10)
# This tells us what each measurement WOULD have been at 10 °C, using the
# fitted Q10 to back out the temperature effect.

d <- d |>
  mutate(
    r10 = reco / Q10_global^((temp - 10) / 10)
  )

# ---- 6. Print the per-measurement R10 values 
cat("--- R10 per measurement ---\n")
d |>
  select(site, plot, day, temp, reco, r10) |>
  mutate(across(c(temp, reco, r10), \(x) round(x, 3))) |>
  print(n = Inf)   # n = Inf prints all 24 rows (default is 10)

# ---- 7. Save the result for later use 
# Write a CSV that you (or your colleague) can pick up downstream for
# correlations with biomass, Cmic, etc.

dir.create("data", showWarnings = FALSE)   # safe if it already exists
write_csv(d, "data/r10_per_measurement.csv")

cat("\nSaved: data/r10_per_measurement.csv\n")
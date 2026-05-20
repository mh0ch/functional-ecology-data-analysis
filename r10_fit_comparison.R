# =============================================================================
# Q10 fitting: compare 3 approaches
#   1. Global pooled fit (all 24 measurements)
#   2. Per-site fit (4 fits, 6 measurements each)
#   3. Within-plot pooled fit (controls for between-plot variation)
# =============================================================================

library(tidyverse)
library(readxl)

# ---- 1. Read & clean -------------------------------------------------------
path   <- "data/NEE.xlsx"
header <- names(read_excel(path, sheet = "ER_GPP", n_max = 0))

d <- read_excel(path, sheet = "ER_GPP", skip = 2, col_names = header) |>
  rename(plot_id = Plot, day = Tag, temp = Bodentemp,
         par = PAR, nee = NEE, reco = ER, gpp = GPP) |>
  mutate(site = substr(plot_id, 1, 1),
         plot = substr(plot_id, 2, 2)) |>
  select(site, plot, plot_id, day, temp, par, reco, gpp)

cat("Temperature range:", round(range(d$temp), 2), "°C\n")
cat("Reco range:", round(range(d$reco), 2), "µmol m⁻² s⁻¹\n\n")

# ---- 2. Global pooled fit --------------------------------------------------
# One Q10 for all 24 measurements. This is the version that gave Q10 ≈ 12,
# inflated because between-site biomass differences get attributed to temp.

fit_global <- nls(reco ~ R10 * Q10^((temp - 10)/10),
                  data = d, start = list(R10 = 2, Q10 = 2))

Q10_global <- coef(fit_global)["Q10"]
R10_global <- coef(fit_global)["R10"]

cat("--- 1. GLOBAL POOLED FIT (n = 24) ---\n")
cat("  R10 =", round(R10_global, 3),
    " Q10 =", round(Q10_global, 3), "\n\n")

# ---- 3. Per-site fits ------------------------------------------------------
# One Q10 per site (6 measurements each). Removes between-site confounding
# but still has within-site (between-plot) confounding. Uses tryCatch in case
# any site's fit fails to converge.

fit_per_site <- d |>
  group_by(site) |>
  group_modify(\(df, ...) {
    f <- tryCatch(
      nls(reco ~ R10 * Q10^((temp - 10)/10),
          data = df, start = list(R10 = 2, Q10 = 2)),
      error = function(e) NULL
    )
    if (is.null(f)) {
      tibble(R10 = NA_real_, Q10 = NA_real_)
    } else {
      tibble(R10 = coef(f)["R10"], Q10 = coef(f)["Q10"])
    }
  })

cat("--- 2. PER-SITE FITS (n = 6 each) ---\n")
fit_per_site |>
  mutate(across(c(R10, Q10), \(x) round(x, 3))) |>
  print()
cat("\n")

# ---- 4. Within-plot pooled fit ---------------------------------------------
# Includes plot_id as a categorical predictor: each plot gets its own
# intercept (= its own log R10), and the temperature slope is estimated
# AFTER controlling for plot identity. So the slope reflects only how Reco
# changes with temperature WITHIN each plot, between the two measurement days.
# This is the cleanest way to isolate the true temperature response.

fit_within <- lm(log(reco) ~ I((temp - 10)/10) + plot_id, data = d)
Q10_within <- exp(coef(fit_within)["I((temp - 10)/10)"])

cat("--- 3. WITHIN-PLOT POOLED FIT (n = 24, 12 within-plot pairs) ---\n")
cat("  Q10 =", round(Q10_within, 3), "\n\n")

# ---- 5. Side-by-side comparison --------------------------------------------
cat("--- COMPARISON ---\n")
tibble(
  method = c("global pooled", "per-site mean",
             "per-site median", "within-plot pooled"),
  Q10    = c(Q10_global,
             mean(fit_per_site$Q10, na.rm = TRUE),
             median(fit_per_site$Q10, na.rm = TRUE),
             Q10_within)
) |>
  mutate(Q10 = round(Q10, 3)) |>
  print()
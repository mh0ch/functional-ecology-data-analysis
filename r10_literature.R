# =============================================================================
# Compute R10 per measurement using a literature Q10 value
# =============================================================================
# Workflow:
#   1. Set the Q10 value to use (configurable at the top)
#   2. Load and clean the Excel data
#   3. Standardize every Reco measurement to 10 °C using the chosen Q10
#   4. Save the result to a CSV
# =============================================================================

# ---- CONFIG: change this value to try a different Q10 ----------------------
Q10 <- 1.83   # literature value for temperate grasslands from https://www.sciencedirect.com/science/article/abs/pii/S016819230500081X

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

# ---- 3. Standardize every measurement to 10 °C -----------------------------
# Rearrange the Q10 equation to back out R10 from each measurement:
#     R10_i = Reco_i / Q10^((T_i - 10)/10)

d <- d |>
  mutate(r10 = reco / Q10^((temp - 10)/10))

# ---- 4. Print the R10 values -----------------------------------------------
cat("--- R10 per measurement (Q10 =", Q10, ") ---\n")
d |>
  select(site, plot, plot_id, day, temp, reco, r10) |>
  mutate(across(c(temp, reco, r10), \(x) round(x, 3))) |>
  print(n = Inf)

# ---- 5. Save to CSV --------------------------------------------------------
dir.create("data", showWarnings = FALSE)

# Include the Q10 value in the filename so different runs don't overwrite
out_file <- sprintf("data/r10_per_measurement_q10_%.1f.csv", Q10)
write_csv(d, out_file)

cat("\nSaved:", out_file, "\n")
cat("Q10 used for standardization:", Q10, "\n")

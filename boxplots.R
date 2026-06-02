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


# reshape to long format first
d_long <- d |>
  pivot_longer(cols = c(reco, gpp),
               names_to = "flux",
               values_to = "value") |>
  mutate(flux = factor(flux, levels = c("gpp", "reco"),
                       labels = c("GPP", "Reco")))

ggplot(d_long, aes(x = site, y = value, fill = flux)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_point(position = position_jitterdodge(jitter.width = 0.1),
             aes(color = flux), show.legend = FALSE) +
  labs(x = "Site", y = "Flux (µmol m⁻² s⁻¹)",
       fill = NULL, title = "GPP and Reco across grassland sites") +
  theme_minimal()
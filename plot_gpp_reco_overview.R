# =============================================================================
# Overview plot: GPP and Reco across 4 sites
# 2 rows (GPP top, Reco bottom), 4 columns (sites A-D)
# Points colored by day, horizontal line for site mean
# Same y-axis scale across all panels
# =============================================================================

library(tidyverse)
library(readxl)

# ---- 1. Load & clean data --------------------------------------------------
path   <- "data/NEE.xlsx"
header <- names(read_excel(path, sheet = "ER_GPP", n_max = 0))

d <- read_excel(path, sheet = "ER_GPP", skip = 2, col_names = header) |>
  rename(plot_id = Plot, day = Tag, temp = Bodentemp,
         par = PAR, nee = NEE, reco = ER, gpp = GPP) |>
  mutate(site = substr(plot_id, 1, 1),
         plot = substr(plot_id, 2, 2))

# ---- 2. Reshape to long format ---------------------------------------------
# One row per measurement per flux type (GPP or Reco)
d_long <- d |>
  pivot_longer(cols = c(gpp, reco),
               names_to  = "flux",
               values_to = "value") |>
  mutate(
    flux = factor(flux,
                  levels = c("gpp", "reco"),
                  labels = c("GPP", "Reco")),
    # Clean up day labels for the legend
    day = case_when(
      day == "Dienstag"   ~ "Tuesday",
      day == "Donnerstag" ~ "Thursday",
      day == "Montag"     ~ "Monday",
      TRUE                ~ day
    )
  )

# ---- 3. Compute site-level means per flux ----------------------------------
d_means <- d_long |>
  group_by(site, flux) |>
  summarise(mean_val = mean(value), .groups = "drop")

# ---- 4. Determine shared y-axis limits -------------------------------------
# Same scale across all panels so GPP > Reco is visually apparent
y_max <- max(d_long$value) * 1.08   # 8% headroom above max value
y_min <- 0

# ---- 5. Build the plot -----------------------------------------------------
p <- ggplot(d_long, aes(x = site, y = value)) +

  # Raw data points — jittered so they don't overlap, colored by day
  geom_jitter(aes(color = day),
              width = 0.15, size = 2.5, alpha = 0.85) +

  # Horizontal mean line per site — drawn as a wide errorbar with no whiskers
  geom_crossbar(data = d_means,
                aes(y = mean_val, ymin = mean_val, ymax = mean_val),
                width = 0.55, linewidth = 0.8, color = "black") +

  # One panel per flux (row) and per site (column)
  facet_grid(flux ~ site, scales = "fixed") +

  # Shared y-axis limits across all panels
  coord_cartesian(ylim = c(y_min, y_max)) +

  # Labels
  labs(
    x     = NULL,
    y     = expression(paste("Flux (µmol CO"[2], " m"^{-2}, " s"^{-1}, ")")),
    color = "Measurement day",
    title = "GPP and Ecosystem Respiration across grassland sites"
  ) +

  # Clean theme suitable for a presentation
  theme_bw(base_size = 13) +
  theme(
    strip.background   = element_rect(fill = "grey92", color = "grey60"),
    strip.text         = element_text(face = "bold", size = 12),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position    = "bottom",
    legend.title       = element_text(face = "bold"),
    plot.title         = element_text(face = "bold", size = 14, hjust = 0.5),
    axis.text.x        = element_blank(),   # site label already in facet strip
    axis.ticks.x       = element_blank()
  ) +

  # Color palette — distinct, colorblind-friendly
  scale_color_manual(values = c(
    "Monday"    = "#E69F00",
    "Tuesday"   = "#56B4E9",
    "Thursday"  = "#009E73"
  ))

# ---- 6. Show and save the plot ---------------------------------------------
# Show in VSCode via httpgd (started automatically by the R extension)
print(p)

# Save as high-resolution PNG for the presentation
ggsave("figures/gpp_reco_overview.png",
       plot   = p,
       width  = 10,
       height = 6,
       dpi    = 300)

cat("Saved: figures/gpp_reco_overview.png\n")

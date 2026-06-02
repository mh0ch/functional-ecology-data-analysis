# =============================================================================
#  Reco & GPP distribution by site — presentation figure
#  Output: figures/reco_gpp_distribution.png
# =============================================================================

library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)

# ---- 1. Load & clean data ---------------------------------------------------
path   <- "data/NEE.xlsx"
header <- names(read_excel(path, sheet = "ER_GPP", n_max = 0))
d <- read_excel(path, sheet = "ER_GPP", skip = 2, col_names = header) |>
  rename(plot_id = Plot, day = Tag, temp = Bodentemp,
         par = PAR, nee = NEE, reco = ER, gpp = GPP) |>
  mutate(site  = substr(plot_id, 1, 1),
         plot  = substr(plot_id, 2, 2),
         day   = as.character(day))

# ---- 2. User-editable colour palette ---------------------------------------
day_colours <- c(
  "Montag"     = "#E57C23",
  "Dienstag"   = "#5176ff",
  "Donnerstag" = "#25ac47"
)

# ---- 3. Reshape to long format ---------------------------------------------
d_long <- d |>
  select(site, plot, day, reco, gpp) |>
  pivot_longer(cols = c(reco, gpp),
               names_to  = "flux",
               values_to = "value") |>
  mutate(
    flux = factor(flux, levels = c("gpp", "reco"),
                  labels = c("GPP", "Reco")),
    site = factor(site)
  )

# ---- 4. Compute means -------------------------------------------------------
means <- d_long |>
  group_by(site, flux) |>
  summarise(mean_val = mean(value, na.rm = TRUE), .groups = "drop")

# ---- 5. Plot ---------------------------------------------------------------
set.seed(1)

site_labels <- c(
  "A" = "A - Saatwiese",
  "B" = "B - Magerwiese",
  "C" = "C - Heißlände",
  "D" = "D - Trockenrasen"
)

p <- ggplot(d_long, aes(x = flux, y = value)) +

  geom_crossbar(
    data    = means,
    aes(y = mean_val, ymin = mean_val, ymax = mean_val),
    width   = 0.45,
    fatten  = 0,
    colour  = "grey25",
    linewidth = 1.2
  ) +

#   geom_vline(
#   xintercept = 1,
#   colour = "#e9e9e9",
#   linewidth = 0.6
# ) +
# geom_vline(
#   xintercept = 2,
#   colour = "#eeeeee",
#   linewidth = 0.6
# ) +

# geom_segment(
#   aes(x = flux, xend = flux, y = 0, yend = Inf),
#   colour = "#e6e6e6",
#   linewidth = 0.4
# ) +

  # --- GPP: filled dots -----------------------------------------------------
  geom_jitter(
    data   = d_long |> filter(flux == "GPP"),
    aes(fill = day, colour = day),
    shape  = 21,
    width  = 0.0,
    height = 0,
    size   = 6,
    alpha  = 0.7,
    stroke = 0.5
  ) +

  # --- Reco: hollow dots (stroke only) --------------------------------------
  geom_jitter(
    data   = d_long |> filter(flux == "Reco"),
    aes(colour = day),
    shape  = 1,
    width  = 0.0,
    height = 0,
    size   = 4.5,
    alpha  = 0.85,
    stroke = 2
  ) +

  facet_wrap(
    ~ site,
    nrow    = 1,
    labeller = labeller(site = site_labels)
  ) +

  scale_y_continuous(
    name   = expression(paste("Flux  (µmol CO"[2], " m"^{-2}, " s"^{-1}, ")")),
    expand = expansion(mult = c(0.08, 0.12))
  ) +

  scale_x_discrete(name = NULL) +

  scale_colour_manual(
    values = day_colours,
    name   = "",
    labels = function(x) paste(x, "     "),
	breaks = c("Montag", "Dienstag", "Donnerstag"),
	guide  = guide_legend(
    override.aes = list(
      shape = 16,   # filled square
      size  = 4
    )
  )
  ) +

  scale_fill_manual(
    values = day_colours,
    guide  = "none"
  ) +

  geom_hline(yintercept = 0, linetype = "dashed",
             colour = "#646464", linewidth = 1) +

  theme_bw(base_size = 13) +
  theme(
    strip.text        = element_text(face = "bold", size = 17),
    strip.background  = element_rect(fill = "grey93", colour = NA),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    axis.text.x       = element_text(size = 17, face = "bold"),
	axis.text.y = element_text(size = 15, face = "bold"),
	axis.title.y = element_text(size = 18),
    legend.position   = "bottom",
    legend.title      = element_text(size = 11),
    legend.text       = element_text(size = 13),
    plot.title        = element_text(face = "bold", size = 14, hjust = 0),
    plot.subtitle     = element_text(size = 11, colour = "grey40", hjust = 0),
    plot.margin       = margin(10, 15, 5, 10)
  )

# ---- 6. Save ---------------------------------------------------------------
ggsave(
  filename = "figures/reco_gpp_distribution.png",
  plot     = p,
  width    = 14,
  height   = 7,
  units    = "in",
  dpi      = 300,
  bg       = "white"
)

message("✓  Figure saved to figures/reco_gpp_distribution.png")
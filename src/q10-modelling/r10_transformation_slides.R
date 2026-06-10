# =============================================================================
# R10 Transformation – Slide Figures
# Four plots:
#   1. Raw Reco vs. Temperature (scatter)
#   2. Same + illustrative Q10 curve overlay
#   3. Temperature-corrected R10 vs. Temperature (scatter)
#   4. Before/after comparison: Reco vs R10 with vertical arrows per point
# =============================================================================

library(ggplot2)
library(dplyr)

dir.create("figures", showWarnings = FALSE)

# Site colours (same palette as other scripts)
site_colours <- c(A = "#E69F00", B = "#0072B2", C = "#009E73", D = "#CC79A7")

# -----------------------------------------------------------------------------
# 1. LOAD DATA
# -----------------------------------------------------------------------------

raw <- read.csv("data/r10_transformation_data_slides.csv",
                header = TRUE, na.strings = c("", "NA"))
df  <- raw[-1, ]   # drop units row

df$reco <- as.numeric(df$reco)
df$r10  <- as.numeric(df$r10)
df$temp <- as.numeric(df$temp)

# Extract site from plot column (first character)
df$site <- substr(df$plot, 1, 1)

cat("Data loaded:", nrow(df), "observations\n")
print(df)


# -----------------------------------------------------------------------------
# 2. SHARED THEME & DIMENSIONS
# -----------------------------------------------------------------------------

W <- 6 * 1.15   # width: 15% wider than before
H <- 4.5

base_theme <- theme_bw(base_size = 13) +
  theme(
    plot.title       = element_text(face = "bold", size = 13),
    legend.position  = "right",
    panel.grid.minor = element_blank()
  )


# -----------------------------------------------------------------------------
# PLOT 1 — Raw Reco vs. Temperature (scatter only, no regression line)
# -----------------------------------------------------------------------------

p1 <- ggplot(df, aes(x = temp, y = reco, colour = site)) +
  geom_point(size = 3.5, stroke = 0) +
  scale_colour_manual(values = site_colours, name = "Site") +
  labs(
    title = "Ecosystem respiration vs. temperature",
    x     = "Soil temperature (\u00b0C)",
    y     = "Reco (\u00b5mol m\u207b\u00b2 s\u207b\u00b9)"
  ) +
  base_theme

ggsave("figures/slide1_reco_vs_temp_scatter.png",
       plot = p1, width = W, height = H, dpi = 150)
cat("Saved: figures/slide1_reco_vs_temp_scatter.png\n")


# # -----------------------------------------------------------------------------
# # PLOT 2 — Same scatter + illustrative Q10 curve overlay
# #
# # Adjust these three values to shift the curve so it visually aligns
# # with your data cloud:
# #   R_ref  — respiration at T_ref (sets the vertical position of the curve)
# #   T_ref  — reference temperature in °C (anchor point on the x-axis)
# #   Q10    — temperature sensitivity (higher = steeper; typical range 1.5–3.0)
# # -----------------------------------------------------------------------------

# R_ref <- 3.0     # <-- adjust: baseline respiration at T_ref
# T_ref <- 10.0    # <-- adjust: reference temperature (°C)
# Q10   <- 2.2     # <-- adjust: Q10 coefficient

# temp_range <- seq(min(df$temp, na.rm = TRUE) - 0.5,
#                   max(df$temp, na.rm = TRUE) + 0.5,
#                   length.out = 200)
# q10_curve  <- data.frame(
#   temp = temp_range,
#   reco = R_ref * Q10^((temp_range - T_ref) / 10)
# )

# p2 <- ggplot(df, aes(x = temp, y = reco)) +
#   geom_line(data = q10_curve,
#             aes(x = temp, y = reco),
#             colour = "grey30", linewidth = 1.1, linetype = "dashed",
#             inherit.aes = FALSE) +
#   geom_point(aes(colour = site), size = 3.5, stroke = 0) +
#   scale_colour_manual(values = site_colours, name = "Site") +
#   annotate("label",
#            x = -Inf, y = Inf, hjust = -0.05, vjust = 1.3,
#            label = sprintf("Q10 curve\nR_ref = %.1f, T_ref = %.0f\u00b0C, Q10 = %.1f",
#                            R_ref, T_ref, Q10),
#            size = 3.3, label.size = 0.3,
#            colour = "grey20", fill = "white", alpha = 0.9) +
#   labs(
#     title = "Ecosystem respiration vs. temperature with Q10 curve",
#     x     = "Soil temperature (\u00b0C)",
#     y     = "Reco (\u00b5mol m\u207b\u00b2 s\u207b\u00b9)"
#   ) +
#   base_theme

# ggsave("figures/slide2_reco_vs_temp_q10curve.png",
#        plot = p2, width = W, height = H, dpi = 150)
# cat("Saved: figures/slide2_reco_vs_temp_q10curve.png\n")


# -----------------------------------------------------------------------------
# PLOT 3 — Temperature-corrected R10 vs. Temperature (scatter)
# -----------------------------------------------------------------------------

# p3 <- ggplot(df, aes(x = temp, y = r10, colour = site)) +
#   geom_point(size = 3.5, stroke = 0) +
#   scale_colour_manual(values = site_colours, name = "Site") +
#   labs(
#     title = "Temperature-corrected R10 vs. temperature",
#     x     = "Soil temperature (\u00b0C)",
#     y     = "R10 (\u00b5mol m\u207b\u00b2 s\u207b\u00b9)"
#   ) +
#   base_theme

# ggsave("figures/slide3_r10_vs_temp_scatter.png",
#        plot = p3, width = W, height = H, dpi = 150)
# cat("Saved: figures/slide3_r10_vs_temp_scatter.png\n")


# -----------------------------------------------------------------------------
# PLOT 4 — Before/after: Reco (open) → R10 (filled), linked by vertical segment
#
# Each point pair shares the same temperature. Vertical segments show how much
# each measurement moved after temperature correction. Segments are coloured
# by site. Open circles = raw Reco; filled circles = R10.
# -----------------------------------------------------------------------------

# Build a long-format data frame for the two point layers
df_before <- df %>% mutate(value = reco, type = "Reco (raw)")
df_after  <- df %>% mutate(value = r10,  type = "R10 (corrected)")
df_long   <- bind_rows(df_before, df_after)

p4 <- ggplot() +
  # Vertical segments connecting Reco to R10 at the same temperature
  geom_segment(data = df,
               aes(x = temp, xend = temp,
                   y = reco,  yend = r10 + 0.05,
                   colour = site),
               linewidth = 0.6, alpha = 0.3) +
  # Raw Reco — open circles
  geom_point(data = df,
             aes(x = temp, y = reco, colour = site),
             shape = 1, size = 2.8, stroke = 0.8, alpha = 0.4) +
  # R10 — filled circles
  geom_point(data = df,
             aes(x = temp, y = r10, colour = site),
             shape = 16, size = 3.5, stroke = 0) +
  scale_colour_manual(values = site_colours, name = "Site") +
  # Manual legend for point types
  annotate("label",
           x = -Inf, y = Inf, hjust = -0.05, vjust = 1.3,
           label = "○  =  Reco (raw)\n●  =  R10 (corrected)",
           size = 3.2, label.size = 0.3,
           colour = "grey20", fill = "white", alpha = 0.9) +
  labs(
    title = "Effect of temperature correction: Reco \u2192 R10",
    x     = "Soil temperature (\u00b0C)",
    y     = "Respiration (\u00b5mol m\u207b\u00b2 s\u207b\u00b9)"
  ) +
  base_theme

ggsave("figures/slide4_reco_vs_r10_correction.png",
       plot = p4, width = W, height = H, dpi = 250)
cat("Saved: figures/slide4_reco_vs_r10_correction.png\n")

cat("\nDone. Four figures saved to ./figures/\n")
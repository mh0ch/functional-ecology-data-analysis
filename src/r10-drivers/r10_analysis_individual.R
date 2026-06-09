# =============================================================================
# Ecosystem Ecology – Respiration Analysis
# Response variable: R10 mean (temperature-corrected ecosystem respiration)
# Predictors: plant diversity, biomass, soil properties, GPP
# =============================================================================

library(ggplot2)
library(dplyr)
library(purrr)

# -----------------------------------------------------------------------------
# 1. LOAD & CLEAN DATA
# -----------------------------------------------------------------------------

# Header from row 1; row 2 is units — drop it
df_raw <- read.csv("data/r10_drivers_data.csv", header = TRUE, na.strings = c("", "NA"))
df     <- df_raw[-1, ]

num_cols <- c("r10_mean",
              "plant_species_richness", "plant_shannon_diversity",
              "shoot_mass", "litter_mass", "root_mass",
              "c_mic", "som", "soil_water_content", "max_whc",
              "gpp_mean")

df[num_cols] <- lapply(df[num_cols], as.numeric)

# Site column (first character of plot_id)
df$site <- substr(df$plot_id, 1, 1)

cat("Data loaded:", nrow(df), "plots across", n_distinct(df$site), "sites\n")
cat("Missing values per column:\n")
print(colSums(is.na(df[num_cols])))


# -----------------------------------------------------------------------------
# 2. DEFINE PREDICTORS
# -----------------------------------------------------------------------------

predictors <- c(
  "plant_species_richness", "plant_shannon_diversity",
  "shoot_mass", "litter_mass", "root_mass",
  "c_mic", "som", "soil_water_content", "max_whc",
  "gpp_mean"
)


# -----------------------------------------------------------------------------
# 3. HELPER FUNCTION: simple linear regression + tidy output
# -----------------------------------------------------------------------------

run_lm <- function(data, response, predictor) {
  d <- data[!is.na(data[[response]]) & !is.na(data[[predictor]]), ]
  n <- nrow(d)

  if (n < 4) {
    return(tibble(
      predictor = predictor, n = n,
      slope = NA, r_squared = NA, p_value = NA, sig = "insufficient data"
    ))
  }

  mod  <- lm(as.formula(paste(response, "~", predictor)), data = d)
  summ <- summary(mod)

  slope <- coef(mod)[2]
  r2    <- summ$r.squared
  p_val <- summ$coefficients[2, 4]

  sig_label <- case_when(
    p_val < 0.001 ~ "***",
    p_val < 0.01  ~ "**",
    p_val < 0.05  ~ "*",
    p_val < 0.10  ~ ".",
    TRUE          ~ "ns"
  )

  tibble(
    predictor = predictor,
    n         = n,
    slope     = round(slope, 4),
    r_squared = round(r2, 3),
    p_value   = round(p_val, 4),
    sig       = sig_label
  )
}


# -----------------------------------------------------------------------------
# 4. RUN ALL REGRESSIONS
# -----------------------------------------------------------------------------

results_all <- map_dfr(predictors, ~ run_lm(df, "r10_mean", .x))

cat("\n============================================================\n")
cat("  REGRESSION RESULTS (response: r10_mean)\n")
cat("  (. = p<0.10, * = p<0.05, ** = p<0.01, *** = p<0.001)\n")
cat("============================================================\n")
print(results_all, n = Inf)


# -----------------------------------------------------------------------------
# 5. INDIVIDUAL SCATTER PLOTS — one PNG per predictor, coloured by site
# -----------------------------------------------------------------------------

dir.create("figures", showWarnings = FALSE)

# Site colours — four visually distinct colours
site_colours <- c(A = "#E69F00", B = "#0072B2", C = "#009E73", D = "#CC79A7")

pretty_label <- function(var) {
  labels <- c(
    plant_species_richness  = "Plant species richness",
    plant_shannon_diversity = "Shannon diversity (H')",
    shoot_mass              = "Shoot biomass (g m\u207b\u00b2)",
    litter_mass             = "Litter biomass (g m\u207b\u00b2)",
    root_mass               = "Root biomass (g m\u207b\u00b2)",
    c_mic                   = "Microbial biomass C (g C m\u207b\u00b2)",
    som                     = "Soil organic matter (%)",
    soil_water_content      = "Soil water content (g H\u2082O g DW\u207b\u00b9)",
    max_whc                 = "Max. water-holding capacity (g H\u2082O g DW\u207b\u00b9)",
    gpp_mean                = "GPP (\u00b5mol m\u207b\u00b2 s\u207b\u00b9)"
  )
  lbl <- labels[var]
  if (is.na(lbl)) var else lbl
}

make_plot <- function(predictor) {

  d <- df %>%
    select(plot_id, site, r10 = r10_mean, x = all_of(predictor)) %>%
    filter(!is.na(r10) & !is.na(x))

  stats <- run_lm(df, "r10_mean", predictor)

  ann <- sprintf("R\u00b2 = %.3f\np = %.4f  %s",
                 stats$r_squared, stats$p_value, stats$sig)

  x_label <- pretty_label(predictor)

  p <- ggplot(d, aes(x = x, y = r10)) +
    # Overall regression line + CI (grey — not tied to any site)
    geom_smooth(method = "lm", se = TRUE,
                colour = "grey40", fill = "grey80",
                linewidth = 0.9, alpha = 0.4) +
    # Points coloured by site
    geom_point(aes(colour = site), size = 3.5, stroke = 0) +
    scale_colour_manual(values = site_colours, name = "Site") +
    # Stats annotation — top left
    annotate("label",
             x = -Inf, y = Inf, hjust = -0.05, vjust = 1.3,
             label = ann, size = 3.5, label.size = 0.3,
             colour = "grey20", fill = "white", alpha = 0.9) +
    labs(
      title = paste("R10 vs.", x_label),
      x     = x_label,
      y     = "R10 (\u00b5mol m\u207b\u00b2 s\u207b\u00b9)"
    ) +
    theme_bw(base_size = 13) +
    theme(
      plot.title       = element_text(face = "bold", size = 13),
      legend.position  = "right",
      panel.grid.minor = element_blank()
    )

  out_path <- file.path("figures", paste0("R10_vs_", predictor, ".png"))
  ggsave(out_path, plot = p, width = 6, height = 4.5, dpi = 150)
  cat("Saved:", out_path, "\n")

  invisible(p)
}

# Save every predictor as its own PNG
walk(predictors, make_plot)

cat("\nDone. All figures saved to ./figures/\n")
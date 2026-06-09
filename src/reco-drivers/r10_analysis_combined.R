# =============================================================================
# Ecosystem Ecology – Respiration Analysis
# Response variable: R10 (temperature-corrected ecosystem respiration)
# Sensitivity analysis: r10_mean vs r10_tuesday
# =============================================================================

library(ggplot2)
library(dplyr)
library(tidyr)
library(purrr)

# -----------------------------------------------------------------------------
# 1. LOAD & CLEAN DATA
# -----------------------------------------------------------------------------

# Re-read properly: header from row 1, skip unit row (row 2)
df_raw <- read.csv("data/r10_drivers_data.csv", header = TRUE, na.strings = c("", "NA"))
# Row 1 in the data is the units row — remove it
df <- df_raw[-1, ]

# Convert all numeric columns
num_cols <- c("r10_mean", "r10_tuesday",
              "plant_species_richness", "plant_shannon_diversity",
              "shoot_mass", "litter_mass", "root_mass",
              "c_mic", "som", "soil_water_content", "max_whc",
              "gpp_mean", "gpp_tuesday")

df[num_cols] <- lapply(df[num_cols], as.numeric)

# Add site column (first character of plot_id)
df$site <- substr(df$plot_id, 1, 1)

cat("Data loaded:", nrow(df), "plots across", n_distinct(df$site), "sites\n")
cat("Missing values per column:\n")
print(colSums(is.na(df[num_cols])))


# -----------------------------------------------------------------------------
# 2. DEFINE PREDICTORS
#    GPP is paired: gpp_mean goes with r10_mean, gpp_tuesday with r10_tuesday
#    All other predictors use the same column for both R10 versions
# -----------------------------------------------------------------------------

standard_predictors <- c(
  "plant_species_richness", "plant_shannon_diversity",
  "shoot_mass", "litter_mass", "root_mass",
  "c_mic", "som", "soil_water_content", "max_whc"
)

# GPP predictors are paired separately (handled below)


# -----------------------------------------------------------------------------
# 3. HELPER FUNCTION: simple linear regression + tidy output
# -----------------------------------------------------------------------------

run_lm <- function(data, response, predictor) {
  d <- data[!is.na(data[[response]]) & !is.na(data[[predictor]]), ]
  n <- nrow(d)

  if (n < 4) {
    return(tibble(
      response  = response, predictor = predictor, n = n,
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
    response  = response,
    predictor = predictor,
    n         = n,
    slope     = round(slope, 4),
    r_squared = round(r2, 3),
    p_value   = round(p_val, 4),
    sig       = sig_label
  )
}


# -----------------------------------------------------------------------------
# 4. RUN REGRESSIONS
# -----------------------------------------------------------------------------

results_standard <- map_dfr(standard_predictors, function(pred) {
  bind_rows(
    run_lm(df, "r10_mean",    pred),
    run_lm(df, "r10_tuesday", pred)
  )
})

results_gpp <- bind_rows(
  run_lm(df, "r10_mean",    "gpp_mean"),
  run_lm(df, "r10_tuesday", "gpp_tuesday")
)

results_all <- bind_rows(results_standard, results_gpp)


# -----------------------------------------------------------------------------
# 5. SENSITIVITY SUMMARY TABLE
#    Wide format: one row per predictor, side-by-side for mean vs tuesday
# -----------------------------------------------------------------------------

sensitivity_table <- results_all %>%
  mutate(
    version = ifelse(grepl("mean", response), "mean", "tuesday"),
    predictor_clean = case_when(
      predictor == "gpp_mean"    ~ "gpp",
      predictor == "gpp_tuesday" ~ "gpp",
      TRUE                       ~ predictor
    )
  ) %>%
  select(predictor_clean, version, r_squared, p_value, sig, slope) %>%
  pivot_wider(
    names_from  = version,
    values_from = c(r_squared, p_value, sig, slope),
    names_glue  = "{version}_{.value}"
  ) %>%
  rename(predictor = predictor_clean) %>%
  select(predictor,
         mean_r_squared, mean_p_value, mean_sig, mean_slope,
         tuesday_r_squared, tuesday_p_value, tuesday_sig, tuesday_slope) %>%
  mutate(
    robust = case_when(
      mean_sig %in% c("*","**","***") & tuesday_sig %in% c("*","**","***") ~ "YES – both sig.",
      mean_sig == "ns" & tuesday_sig == "ns"                               ~ "YES – both ns",
      TRUE                                                                  ~ "NO – diverges"
    )
  )

cat("\n============================================================\n")
cat("  SENSITIVITY SUMMARY TABLE\n")
cat("  (. = p<0.10, * = p<0.05, ** = p<0.01, *** = p<0.001)\n")
cat("============================================================\n")
print(sensitivity_table, width = 130, n = Inf)


# -----------------------------------------------------------------------------
# 6. INDIVIDUAL SCATTER PLOTS – stored as list, then combined into one image
# -----------------------------------------------------------------------------

dir.create("figures", showWarnings = FALSE)

col_mean    <- "#2166ac"
col_tuesday <- "#d6604d"

pretty_label <- function(var) {
  labels <- c(
    plant_species_richness = "Plant species richness",
    plant_shannon_diversity = "Shannon diversity (H')",
    shoot_mass              = "Shoot biomass (g m\u207b\u00b2)",
    litter_mass             = "Litter biomass (g m\u207b\u00b2)",
    root_mass               = "Root biomass (g m\u207b\u00b2)",
    c_mic                   = "Microbial biomass C (g C m\u207b\u00b2)",
    som                     = "Soil organic matter (%)",
    soil_water_content      = "Soil water content (g H\u2082O g DW\u207b\u00b9)",
    max_whc                 = "Max. WHC (g H\u2082O g DW\u207b\u00b9)",
    gpp_mean                = "GPP (paired; \u00b5mol m\u207b\u00b2 s\u207b\u00b9)"
  )
  lbl <- labels[var]
  if (is.na(lbl)) var else lbl
}

make_plot <- function(pred_mean, pred_tuesday = pred_mean) {

  d_mean <- df %>%
    select(plot_id, site, r10 = r10_mean, x = all_of(pred_mean)) %>%
    mutate(version = "r10_mean")

  d_tue <- df %>%
    select(plot_id, site, r10 = r10_tuesday, x = all_of(pred_tuesday)) %>%
    mutate(version = "r10_tuesday")

  d_long <- bind_rows(d_mean, d_tue) %>% filter(!is.na(r10) & !is.na(x))

  stats_mean <- run_lm(df, "r10_mean", pred_mean)
  stats_tue  <- run_lm(df, "r10_tuesday", pred_tuesday)

  ann <- sprintf("mean:    R\u00b2=%.3f, p=%s\ntuesday: R\u00b2=%.3f, p=%s",
                 stats_mean$r_squared, stats_mean$sig,
                 stats_tue$r_squared,  stats_tue$sig)

  x_label    <- pretty_label(pred_mean)
  is_gpp     <- (pred_mean != pred_tuesday)
  plot_title <- if (is_gpp) "GPP (paired)" else x_label

  ggplot(d_long, aes(x = x, y = r10, colour = version, fill = version)) +
    geom_smooth(method = "lm", se = TRUE, alpha = 0.15, linewidth = 0.8) +
    geom_point(aes(shape = site), size = 2.5, stroke = 0.7) +
    scale_colour_manual(
      values = c(r10_mean = col_mean, r10_tuesday = col_tuesday),
      labels = c(r10_mean = "R10 mean", r10_tuesday = "R10 Tue"),
      name   = NULL
    ) +
    scale_fill_manual(
      values = c(r10_mean = col_mean, r10_tuesday = col_tuesday),
      guide  = "none"
    ) +
    scale_shape_manual(
      values = c(A = 16, B = 17, C = 15, D = 18),
      name   = "Site"
    ) +
    annotate("label",
             x = -Inf, y = Inf, hjust = -0.04, vjust = 1.3,
             label = ann, size = 2.6, label.size = 0.25,
             colour = "grey20", fill = "white", alpha = 0.85) +
    labs(title = plot_title,
         x = x_label,
         y = "R10 (\u00b5mol m\u207b\u00b2 s\u207b\u00b9)") +
    theme_bw(base_size = 10) +
    theme(
      plot.title       = element_text(face = "bold", size = 10),
      legend.position  = "bottom",
      legend.key.size  = unit(0.4, "cm"),
      legend.text      = element_text(size = 8),
      legend.box       = "horizontal",
      panel.grid.minor = element_blank(),
      axis.title       = element_text(size = 8),
      axis.text        = element_text(size = 7)
    )
}

# Build all individual plots into a named list
plot_list <- map(standard_predictors, function(pred) {
  make_plot(pred_mean = pred, pred_tuesday = pred)
})
names(plot_list) <- standard_predictors

# Add GPP paired plot
plot_list[["gpp_paired"]] <- make_plot(pred_mean = "gpp_mean",
                                       pred_tuesday = "gpp_tuesday")


# -----------------------------------------------------------------------------
# 7. COMBINED SCATTER GRID  (all predictors in one image)
# -----------------------------------------------------------------------------

if (!requireNamespace("patchwork", quietly = TRUE)) install.packages("patchwork")
library(patchwork)

# 10 plots → 2 rows × 5 columns
combined_scatters <- wrap_plots(plot_list, ncol = 5) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

combined_scatters <- combined_scatters +
  plot_annotation(
    title    = "R10 vs. all predictors – sensitivity: mean (blue) vs. Tuesday (red)",
    subtitle = "Filled band = 95% CI of regression | Point shape = site (A/B/C/D) | Stats annotated per panel",
    theme    = theme(
      plot.title    = element_text(face = "bold", size = 13),
      plot.subtitle = element_text(size = 9, colour = "grey40")
    )
  )

ggsave("figures/all_predictors_scatter_grid.png",
       plot   = combined_scatters,
       width  = 20, height = 9, dpi = 150)
cat("Saved: figures/all_predictors_scatter_grid.png\n")


# -----------------------------------------------------------------------------
# 8. SENSITIVITY OVERVIEW PANEL  (R² dot-plot + standardised slopes)
# -----------------------------------------------------------------------------

# All predictors including the three new ones
all_pred_labels <- c(
  plant_species_richness  = "Species richness",
  plant_shannon_diversity = "Shannon H'",
  shoot_mass              = "Shoot biomass",
  litter_mass             = "Litter biomass",
  root_mass               = "Root biomass",
  c_mic                   = "Microbial C",
  som                     = "SOM",
  soil_water_content      = "Soil water",
  max_whc                 = "Max. WHC",
  gpp_mean                = "GPP (paired)",
  gpp_tuesday             = "GPP (paired)"
)

dot_data <- results_all %>%
  mutate(
    version = ifelse(grepl("mean", response), "r10_mean", "r10_tuesday"),
    predictor_label = all_pred_labels[predictor],
    significant = !is.na(p_value) & p_value < 0.05
  )

pred_order <- dot_data %>%
  group_by(predictor_label) %>%
  summarise(mean_r2 = mean(r_squared, na.rm = TRUE)) %>%
  arrange(desc(mean_r2)) %>%
  pull(predictor_label)

dot_data$predictor_label <- factor(dot_data$predictor_label, levels = rev(pred_order))

p_r2 <- ggplot(dot_data,
               aes(x = r_squared, y = predictor_label,
                   colour = version, shape = significant)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey70") +
  geom_point(size = 4, stroke = 0.9,
             position = position_dodge(width = 0.4)) +
  scale_colour_manual(values = c(r10_mean = col_mean, r10_tuesday = col_tuesday),
                      labels = c("R10 mean", "R10 Tuesday"), name = "R10 version") +
  scale_shape_manual(values = c("TRUE" = 16, "FALSE" = 1),
                     labels = c("TRUE" = "p < 0.05", "FALSE" = "p \u2265 0.05"),
                     name = "Significance") +
  labs(title = "A   Explanatory power (R\u00b2)", x = "R\u00b2", y = NULL) +
  xlim(0, 1) +
  theme_bw(base_size = 11) +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold"),
        legend.position = "bottom", legend.box = "vertical")

# Standardised slopes table — include all predictors
all_pred_cols <- c(standard_predictors, "gpp_mean", "gpp_tuesday")
sd_table <- tibble(
  predictor = all_pred_cols,
  sd_pred   = sapply(all_pred_cols, function(v) sd(df[[v]], na.rm = TRUE))
)

dot_data2 <- results_all %>%
  mutate(
    version = ifelse(grepl("mean", response), "r10_mean", "r10_tuesday"),
    predictor_label = all_pred_labels[predictor],
    significant = !is.na(p_value) & p_value < 0.05
  ) %>%
  left_join(sd_table, by = "predictor") %>%
  mutate(
    std_slope = slope * sd_pred,
    predictor_label = factor(predictor_label, levels = rev(pred_order))
  )

p_slope <- ggplot(dot_data2,
                  aes(x = std_slope, y = predictor_label,
                      colour = version, shape = significant)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey70") +
  geom_point(size = 4, stroke = 0.9,
             position = position_dodge(width = 0.4)) +
  scale_colour_manual(values = c(r10_mean = col_mean, r10_tuesday = col_tuesday),
                      labels = c("R10 mean", "R10 Tuesday"), name = "R10 version") +
  scale_shape_manual(values = c("TRUE" = 16, "FALSE" = 1),
                     labels = c("TRUE" = "p < 0.05", "FALSE" = "p \u2265 0.05"),
                     name = "Significance") +
  labs(title = "B   Standardised slope (per 1 SD of predictor)",
       x = "\u0394 R10 (\u00b5mol m\u207b\u00b2 s\u207b\u00b9) per SD", y = NULL) +
  theme_bw(base_size = 11) +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold"),
        legend.position = "bottom", legend.box = "vertical")

overview_panel <- (p_r2 | p_slope) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

overview_panel <- overview_panel +
  plot_annotation(
    title    = "Respiration Drivers \u2013 Sensitivity Overview",
    subtitle = "Filled = p < 0.05 | Open = p \u2265 0.05 | Predictors ordered by mean R\u00b2 | Points dodged by R10 version",
    theme    = theme(plot.title    = element_text(face = "bold", size = 14),
                     plot.subtitle = element_text(size = 9, colour = "grey40"))
  )

ggsave("figures/overview_sensitivity_panel.png",
       plot = overview_panel, width = 14, height = 7, dpi = 150)
cat("Saved: figures/overview_sensitivity_panel.png\n")


# -----------------------------------------------------------------------------
# 9. PRINT FULL RESULTS TO CONSOLE
# -----------------------------------------------------------------------------

cat("\n============================================================\n")
cat("  FULL REGRESSION RESULTS\n")
cat("============================================================\n")
print(results_all %>% arrange(predictor, response), n = Inf)

cat("\n============================================================\n")
cat("  SENSITIVITY SUMMARY (wide format)\n")
cat("============================================================\n")
print(sensitivity_table, n = Inf, width = 130)

cat("\nDone. Figures saved to ./figures/\n")

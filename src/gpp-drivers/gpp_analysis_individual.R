# =============================================================================
# Ecosystem Ecology – GPP Driver Analysis
# File 1: gpp_drivers_data_tuesday.csv  → GPP ~ each predictor (Tuesday only)
# File 2: gpp_drivers_data_allDays.csv  → GPP ~ PAR (all days, shape = day)
# Points coloured by site (A/B/C/D) throughout
# =============================================================================

library(ggplot2)
library(dplyr)
library(purrr)

dir.create("figures", showWarnings = FALSE)

# Site colours (colourblind-friendly)
site_colours <- c(A = "#E69F00", B = "#0072B2", C = "#009E73", D = "#CC79A7")

# Day shapes
day_shapes <- c(Dienstag = 16, Montag = 15, Donnerstag = 17)   # circle, square, triangle


# -----------------------------------------------------------------------------
# HELPER: run simple linear regression, return tidy stats
# -----------------------------------------------------------------------------

run_lm <- function(data, response, predictor) {
  d <- data[!is.na(data[[response]]) & !is.na(data[[predictor]]), ]
  n <- nrow(d)

  if (n < 4) {
    return(list(r_squared = NA, p_value = NA, sig = "insufficient data",
                slope = NA, n = n))
  }

  mod  <- lm(as.formula(paste(response, "~", predictor)), data = d)
  summ <- summary(mod)
  p_val <- summ$coefficients[2, 4]

  sig_label <- case_when(
    p_val < 0.001 ~ "***",
    p_val < 0.01  ~ "**",
    p_val < 0.05  ~ "*",
    p_val < 0.10  ~ ".",
    TRUE          ~ "ns"
  )

  list(
    r_squared = round(summ$r.squared, 3),
    p_value   = round(p_val, 4),
    sig       = sig_label,
    slope     = round(coef(mod)[2], 4),
    n         = n
  )
}


# -----------------------------------------------------------------------------
# HELPER: pretty x-axis labels
# -----------------------------------------------------------------------------

pretty_label <- function(var) {
  labels <- c(
    shannon_index       = "Shannon diversity (H')",
    legu_biomass        = "Legume biomass (g m\u207b\u00b2)",
    legu_biomass_share  = "Legume biomass share (%)",
    LAI                 = "Leaf area index",
    PAR                 = "PAR (\u00b5mol m\u207b\u00b2 s\u207b\u00b9)",
    shoot_biomass       = "Shoot biomass (g m\u207b\u00b2)"
  )
  lbl <- labels[var]
  if (is.na(lbl)) var else lbl
}


# =============================================================================
# PART 1 — TUESDAY DATA: GPP ~ each predictor, coloured by site
# =============================================================================

tue_raw <- read.csv("data/gpp_drivers_data_tuesday.csv",
                    header = TRUE, na.strings = c("", "NA"))
tue     <- tue_raw[-1, ]   # drop units row

# Convert numeric columns
tue_num <- c("GPP", "shannon_index", "legu_biomass", "legu_biomass_share",
             "LAI", "PAR", "shoot_biomass")
tue[tue_num] <- lapply(tue[tue_num], as.numeric)

# Extract site from plot column (first character)
tue$site <- substr(tue$plot, 1, 1)

cat("Tuesday data loaded:", nrow(tue), "plots\n")
cat("Missing values:\n"); print(colSums(is.na(tue[tue_num])))

# Predictors for Tuesday file
tue_predictors <- c("shannon_index", "legu_biomass", "legu_biomass_share",
                    "LAI", "PAR", "shoot_biomass")

# Plot function for Tuesday data
make_plot_tue <- function(predictor) {

  d <- tue %>%
    select(plot, site, gpp = GPP, x = all_of(predictor)) %>%
    filter(!is.na(gpp) & !is.na(x))

  stats <- run_lm(d, "gpp", "x")

  ann <- sprintf("R\u00b2 = %.3f\np = %.4f  %s",
                 stats$r_squared, stats$p_value, stats$sig)

  x_label <- pretty_label(predictor)

  p <- ggplot(d, aes(x = x, y = gpp)) +
    geom_smooth(method = "lm", se = TRUE,
                colour = "grey40", fill = "grey80",
                linewidth = 0.9, alpha = 0.4) +
    geom_point(aes(colour = site), size = 3.5, stroke = 0) +
    scale_colour_manual(values = site_colours, name = "Site") +
    annotate("label",
             x = -Inf, y = Inf, hjust = -0.05, vjust = 1.3,
             label = ann, size = 3.5, label.size = 0.3,
             colour = "grey20", fill = "white", alpha = 0.9) +
    labs(
      title = paste("GPP vs.", x_label),
      x     = x_label,
      y     = "GPP (\u00b5mol CO\u2082 m\u207b\u00b2 s\u207b\u00b9)"
    ) +
    theme_bw(base_size = 13) +
    theme(
      plot.title       = element_text(face = "bold", size = 13),
      legend.position  = "right",
      panel.grid.minor = element_blank()
    )

  out_path <- file.path("figures", paste0("GPP_vs_", predictor, "_tuesday.png"))
  ggsave(out_path, plot = p, width = 6, height = 4.5, dpi = 150)
  cat("Saved:", out_path, "\n")
  invisible(p)
}

walk(tue_predictors, make_plot_tue)

# Print Tuesday regression results
cat("\n============================================================\n")
cat("  GPP DRIVER RESULTS — Tuesday data\n")
cat("  (. = p<0.10, * = p<0.05, ** = p<0.01, *** = p<0.001)\n")
cat("============================================================\n")
results_tue <- map_dfr(tue_predictors, function(pred) {
  d <- tue %>% select(gpp = GPP, x = all_of(pred)) %>% filter(!is.na(gpp) & !is.na(x))
  s <- run_lm(d, "gpp", "x")
  tibble(predictor = pred, n = s$n, slope = s$slope,
         r_squared = s$r_squared, p_value = s$p_value, sig = s$sig)
})
print(results_tue, n = Inf)


# =============================================================================
# PART 2 — ALL DAYS DATA: GPP ~ PAR, colour = site, shape = day
# =============================================================================

all_raw <- read.csv("data/gpp_drivers_data_allDays.csv",
                    header = TRUE, na.strings = c("", "NA"))
all_d   <- all_raw[-1, ]   # drop units row

all_d$PAR <- as.numeric(all_d$PAR)
all_d$GPP <- as.numeric(all_d$GPP)
all_d$site <- substr(all_d$plot, 1, 1)

# Normalise day names (file may use German names)
all_d$day <- trimws(all_d$day)

cat("\nAll-days data loaded:", nrow(all_d), "observations\n")
cat("Days present:", paste(unique(all_d$day), collapse = ", "), "\n")
cat("Missing values: PAR =", sum(is.na(all_d$PAR)),
    "| GPP =", sum(is.na(all_d$GPP)), "\n")

# Map whatever day names appear to shapes robustly
unique_days  <- sort(unique(all_d$day))
shape_values <- c(16, 15, 17, 18)[seq_along(unique_days)]   # circle, square, triangle, diamond
names(shape_values) <- unique_days

stats_all <- run_lm(all_d, "GPP", "PAR")
ann_all   <- sprintf("R\u00b2 = %.3f\np = %.4f  %s",
                     stats_all$r_squared, stats_all$p_value, stats_all$sig)

p_all <- ggplot(all_d, aes(x = PAR, y = GPP)) +
  geom_smooth(method = "lm", se = TRUE,
              colour = "grey40", fill = "grey80",
              linewidth = 0.9, alpha = 0.4) +
  geom_point(aes(colour = site, shape = day), size = 3.5, stroke = 0.5) +
  scale_colour_manual(values = site_colours, name = "Site") +
  scale_shape_manual(values = shape_values, name = "Day") +
  annotate("label",
           x = -Inf, y = Inf, hjust = -0.05, vjust = 1.3,
           label = ann_all, size = 3.5, label.size = 0.3,
           colour = "grey20", fill = "white", alpha = 0.9) +
  labs(
    title = "GPP vs. PAR (all days)",
    x     = "PAR (\u00b5mol m\u207b\u00b2 s\u207b\u00b9)",
    y     = "GPP (\u00b5mol CO\u2082 m\u207b\u00b2 s\u207b\u00b9)"
  ) +
  theme_bw(base_size = 13) +
  theme(
    plot.title       = element_text(face = "bold", size = 13),
    legend.position  = "right",
    panel.grid.minor = element_blank()
  )

ggsave("figures/GPP_vs_PAR_allDays.png",
       plot = p_all, width = 6, height = 4.5, dpi = 150)
cat("Saved: figures/GPP_vs_PAR_allDays.png\n")

cat("\n============================================================\n")
cat("  GPP ~ PAR RESULT — all days\n")
cat("============================================================\n")
cat(sprintf("  R² = %.3f | p = %.4f | %s | n = %d\n",
            stats_all$r_squared, stats_all$p_value,
            stats_all$sig, stats_all$n))

cat("\nDone. All figures saved to ./figures/\n")
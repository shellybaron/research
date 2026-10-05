# =============================================================================
# figureS1.R
# Figure S1 (supplementary). Linearity check: mean signed error by decile of maternal weight
# at delivery, adjusted for maternal height, with a linear fit overlaid.
# Supports the choice of a linear (not polynomial/spline) term for weight
# in Tables 2 and S2. Requested during review (originally phrased as a question
# about the "linearity of the weight distribution").
# =============================================================================

source("cohort_setup.R")
library(ggplot2)
library(officer)

res    <- build_cohort()
cohort <- res$cohort

# ---- height-adjust the signed error (partial out height's linear effect) ---
m_h <- lm(signed_err ~ height_10cm, data = cohort)
cohort$err_height_adj <- residuals(m_h)

# ---- 10 equal-sized deciles by weight at delivery ---------------------------
cohort$wdec <- dplyr::ntile(cohort$weight_current, 10)

dec_summary <- cohort %>%
  dplyr::group_by(wdec) %>%
  dplyr::summarise(
    w_med   = median(weight_current),
    err_adj = mean(err_height_adj),
    n       = dplyr::n(),
    .groups = "drop"
  )

fit   <- lm(err_adj ~ w_med, data = dec_summary)
r_val <- sqrt(summary(fit)$r.squared)
cat(sprintf("Linear fit through decile means: slope = %.4f per kg (%.2f per 10 kg), r = %.3f\n",
            coef(fit)[2], coef(fit)[2] * 10, r_val))

# ---- quadratic / cubic terms on the full (non-binned) data, for the caption -
m_lin  <- lm(signed_err ~ weight_10kg, data = cohort)
m_quad <- lm(signed_err ~ weight_10kg + I(weight_10kg^2), data = cohort)
m_cub  <- lm(signed_err ~ weight_10kg + I(weight_10kg^2) + I(weight_10kg^3), data = cohort)
p_quad <- summary(m_quad)$coefficients["I(weight_10kg^2)", "Pr(>|t|)"]
p_cub  <- summary(m_cub)$coefficients["I(weight_10kg^3)", "Pr(>|t|)"]
cat(sprintf("Quadratic term p = %.3g | Cubic term p = %.3g | R2 (linear/quad/cubic) = %.4f / %.4f / %.4f\n",
            p_quad, p_cub, summary(m_lin)$r.squared, summary(m_quad)$r.squared, summary(m_cub)$r.squared))

# ---- plot --------------------------------------------------------------------
n_per_decile <- round(nrow(cohort) / 10)

p <- ggplot(dec_summary, aes(x = w_med, y = err_adj)) +
  geom_smooth(method = "lm", se = FALSE, color = "grey50",
              linewidth = 0.4, linetype = "dashed") +
  geom_point(size = 5, color = "#1f6fb4") +
  geom_text(aes(label = wdec), color = "white", size = 2.8, fontface = "bold") +
  labs(
    x = "Weight at delivery (kg, decile median)",
    y = "Mean signed error (%, height-adjusted)",
    subtitle = sprintf("Each point = one decile of maternal weight (~%s women); number shows decile rank (1 = lightest)",
                        format(n_per_decile, big.mark = ","))
  ) +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(),
        plot.subtitle = element_text(size = 8, color = "grey30"))

ggsave(file.path(OUTPUT_DIR, "figureS1.png"), p, width = 7, height = 4.8, dpi = 300)
cat("figureS1.png written\n")

# ---- Word document with the figure and caption -------------------------------
caption_txt <- paste0(
  "Figure S1. Linearity of the association between maternal weight at delivery and clinical estimation error. ",
  "Each point shows the mean signed error, adjusted for height, for one of ten equal-sized weight deciles ",
  "(n = ", format(nrow(cohort), big.mark = ","), " deliveries); the dashed line is the linear fit across deciles ",
  "(r = ", sprintf("%.2f", r_val), ")."
)

fontname <- "Times New Roman"

doc <- read_docx() %>%
  body_add_img(src = file.path(OUTPUT_DIR, "figureS1.png"), width = 7, height = 4.8) %>%
  body_add_fpar(fpar(
    ftext("Figure S1.", fp_text(font.family = fontname, bold = TRUE)),
    ftext(substring(caption_txt, 11), fp_text(font.family = fontname))
  ))

print(doc, target = file.path(OUTPUT_DIR, "figureS1.docx"))

register_figure("figureS1", file.path(OUTPUT_DIR, "figureS1.png"), 7, 4.8,
                "Figure S1.", substring(caption_txt, 11))

cat("figureS1.docx written\n")

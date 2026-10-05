# =============================================================================
# tableS2.R
# Table S2 (supplementary). Sensitivity analysis: repeats Table 2 exactly, substituting
# pre-pregnancy weight for weight at delivery, to confirm the weight
# association is not an artifact of gestational weight gain. Requested during review.
# =============================================================================

source("cohort_setup.R")

res    <- build_cohort()
cohort <- res$cohort

fmt3 <- function(x) sprintf("%.3f", x)

# ---- restrict to complete, plausible pre-pregnancy weight ------------------
cohort <- cohort %>%
  filter(!is.na(weight_before),
         weight_before >= 35, weight_before <= 200) %>%
  mutate(
    weight_before_10kg = weight_before / 10,
    BMI_before          = weight_before / (height_cm / 100)^2,
    BMI_before_5        = BMI_before / 5
  )

n_sens      <- nrow(cohort)
pct_of_full <- round(100 * n_sens / nrow(res$cohort), 1)
cat(sprintf("Table S2 cohort (pre-pregnancy weight): n = %d (%.1f%% of primary cohort)\n",
            n_sens, pct_of_full))

fit_row <- function(formula_str, exposure_label) {
  m  <- lm(as.formula(formula_str), data = cohort)
  cl <- lm_cluster(m, cohort$id)
  term <- names(cl$est)[length(cl$est)]
  data.frame(
    Exposure = exposure_label,
    Beta = sprintf("%+.2f", cl$est[term]),
    CI   = sprintf("%.2f, %.2f", cl$lo[term], cl$hi[term]),
    R2   = fmt3(summary(m)$r.squared),
    p    = ifelse(cl$p[term] < 0.001, "<0.001", fmt3(cl$p[term])),
    stringsAsFactors = FALSE
  )
}

# Joint model (sensitivity analysis): height + pre-pregnancy weight together
m_joint <- lm(signed_err ~ height_10cm + weight_before_10kg, data = cohort)
r2_joint <- summary(m_joint)$r.squared
cl <- lm_cluster(m_joint, cohort$id)

row_h <- data.frame(
  Exposure = "Joint model, Height, per 10 cm",
  Beta = sprintf("%+.2f", cl$est["height_10cm"]),
  CI   = sprintf("%.2f, %.2f", cl$lo["height_10cm"], cl$hi["height_10cm"]),
  R2   = fmt3(r2_joint),
  p    = ifelse(cl$p["height_10cm"] < 0.001, "<0.001", fmt3(cl$p["height_10cm"])),
  stringsAsFactors = FALSE
)
row_w <- data.frame(
  Exposure = "Joint model, Pre-pregnancy weight, per 10 kg",
  Beta = sprintf("%+.2f", cl$est["weight_before_10kg"]),
  CI   = sprintf("%.2f, %.2f", cl$lo["weight_before_10kg"], cl$hi["weight_before_10kg"]),
  R2   = "",
  p    = ifelse(cl$p["weight_before_10kg"] < 0.001, "<0.001", fmt3(cl$p["weight_before_10kg"])),
  stringsAsFactors = FALSE
)

bmi_row    <- fit_row("signed_err ~ BMI_before_5",       "Pre-pregnancy BMI, per 5 kg/m\u00b2")
height_row <- fit_row("signed_err ~ height_10cm",        "Alone, Height, per 10 cm")
weight_row <- fit_row("signed_err ~ weight_before_10kg", "Alone, Pre-pregnancy weight, per 10 kg")

tableS2_df <- rbind(row_h, row_w, bmi_row, height_row, weight_row)
colnames(tableS2_df) <- c("Exposure", "Beta", "95% CI", "R\u00b2", "p-value")

print(tableS2_df)

caption_txt <- sprintf("Table S2. Linear regression of maternal height and pre-pregnancy weight on signed percentage error: sensitivity analysis among deliveries with available pre-pregnancy weight (n = %s; %.1f%% of the analytic cohort)",
                       format(n_sens, big.mark = ","), pct_of_full)

footnote_txt <- paste(
  "Beta is expressed in percentage points of signed error; negative values indicate underestimation.",
  "Height and pre-pregnancy weight were entered jointly in the sensitivity model; pre-pregnancy BMI, height alone,",
  "and pre-pregnancy weight alone are supporting models. R\u00b2 for the joint model refers to the full",
  "two-variable model. 95% CIs and p-values use cluster-robust standard errors (clustered by woman)."
)

make_table_docx(
  table_df     = tableS2_df,
  caption_txt  = caption_txt,
  footnote_txt = footnote_txt,
  path         = file.path(OUTPUT_DIR, "tableS2.docx")
)

cat("tableS2.docx written\n")

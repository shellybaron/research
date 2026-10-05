# =============================================================================
# tableS3.R
# Tables S3a and S3b (supplementary). Does maternal height/weight reflect true
# fetal size? Two outputs from one script:
#   S3a: actual birth weight as the outcome (reference point).
#   S3b: clinical weight estimate as the outcome, with and without adjustment
#        for birth weight (diagnostic check).
# Requested during review. Split into two tables so that each has a single,
# uniform outcome (no merged cells).
#
# NOTE: this is the ONE deliberate exception to the locked constraint that
# birth weight never appears as a model covariate (see cohort_setup.R /
# manuscript framing). It is included ONLY as a diagnostic sensitivity check,
# clearly separated into the supplement.
# =============================================================================

source("cohort_setup.R")

res    <- build_cohort()
cohort <- res$cohort

fmt3 <- function(x) sprintf("%.3f", x)

fit_row <- function(formula_str, exposure_label, adjusted_for, term, data = cohort) {
  m  <- lm(as.formula(formula_str), data = data)
  cl <- lm_cluster(m, data$id)   # cluster-robust by woman
  data.frame(
    Exposure     = exposure_label,
    `Adjusted for` = adjusted_for,
    Beta = sprintf("%+.1f", cl$est[term]),
    CI   = sprintf("%.1f, %.1f", cl$lo[term], cl$hi[term]),
    p    = ifelse(cl$p[term] < 0.001, "<0.001", fmt3(cl$p[term])),
    check.names = FALSE, stringsAsFactors = FALSE
  )
}

# ------------------------------ Table S3a -------------------------------------
row_h <- fit_row("birth_weight ~ height_10cm + weight_10kg",
                  "Height, per 10 cm", "Maternal weight", "height_10cm")
row_w <- fit_row("birth_weight ~ height_10cm + weight_10kg",
                  "Weight, per 10 kg", "Maternal height", "weight_10kg")

tableS3a_df <- rbind(row_h, row_w)
colnames(tableS3a_df) <- c("Exposure", "Adjusted for", "Beta (g)", "95% CI", "p-value")

print(tableS3a_df)

caption_txt <- "Table S3a. Does maternal height and weight predict actual birth weight?"

footnote_txt <- paste(
  "Outcome: birth weight (g). Beta is in grams. Height and weight were entered jointly. Taller and",
  "heavier mothers delivered larger infants, as expected; this model is the reference point for Table S3b.",
  "95% CIs and p-values use cluster-robust standard errors (clustered by woman)."
)

make_table_docx(
  table_df     = tableS3a_df,
  caption_txt  = caption_txt,
  footnote_txt = footnote_txt,
  path         = file.path(OUTPUT_DIR, "tableS3a.docx")
)

cat("tableS3a.docx written\n")

# ------------------------------ Table S3b -------------------------------------
# Without adjusting for birth weight (matches the primary analysis's covariates)
row_h_unadj <- fit_row("clinical_weight_estimate ~ height_10cm + weight_10kg",
                       "Height, per 10 cm", "Maternal weight", "height_10cm")
row_w_unadj <- fit_row("clinical_weight_estimate ~ height_10cm + weight_10kg",
                       "Weight, per 10 kg", "Maternal height", "weight_10kg")

# Additionally adjusting for birth weight (diagnostic sensitivity check)
row_h_adj <- fit_row("clinical_weight_estimate ~ height_10cm + weight_10kg + birth_weight",
                     "Height, per 10 cm", "Maternal weight, Birth weight", "height_10cm")
row_w_adj <- fit_row("clinical_weight_estimate ~ height_10cm + weight_10kg + birth_weight",
                     "Weight, per 10 kg", "Maternal height, Birth weight", "weight_10kg")

tableS3b_df <- rbind(row_h_unadj, row_h_adj, row_w_unadj, row_w_adj)
colnames(tableS3b_df) <- c("Exposure", "Adjusted for", "Beta (g)", "95% CI", "p-value")

print(tableS3b_df)

caption_txt <- "Table S3b. Clinical weight estimate as the outcome, with and without adjustment for birth weight"

footnote_txt <- paste(
  "Outcome: clinical weight estimate (g); Beta in grams. \"Adjusted for\" lists the other variables in",
  "that row's model. Unadjusted rows match the primary analysis; adjusted rows add birth weight as a diagnostic",
  "check, to test whether the associations reflect true fetal size (see Table S3a). After accounting for birth",
  "weight, greater maternal height was associated with a lower clinical weight estimate and greater maternal",
  "weight with a higher one. 95% CIs and p-values use cluster-robust standard errors (clustered by woman)."
)

make_table_docx(
  table_df     = tableS3b_df,
  caption_txt  = caption_txt,
  footnote_txt = footnote_txt,
  path         = file.path(OUTPUT_DIR, "tableS3b.docx")
)

cat("tableS3b.docx written\n")

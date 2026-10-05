# =============================================================================
# table3_clinical.R
# Table 3. Multinomial logistic regression, reference = within +/-10%,
# in clinical units (per 10 cm / per 10 kg / per 5 kg/m2).
# =============================================================================

source("cohort_setup.R")
library(officer)
library(flextable)
library(nnet)

res    <- build_cohort()
cohort <- res$cohort
cohort$direction <- relevel(cohort$direction, ref = "Within")

fmt2 <- function(x) sprintf("%.2f", x)

# Joint model: height + weight
# maxit/reltol tightened beyond nnet defaults to ensure full MLE convergence
m_joint <- multinom(direction ~ height_10cm + weight_10kg, data = cohort,
                     trace = FALSE, maxit = 1000, reltol = 1e-12, model = TRUE)
cf <- coef(m_joint)
# Cluster-robust (by woman) SEs, reshaped to the same layout as coef()
se_clustered <- function(model, cluster) {
  V  <- cluster_vcov(model, cluster)
  cm <- coef(model)
  se <- cm
  for (r in rownames(cm)) for (cn in colnames(cm))
    se[r, cn] <- sqrt(V[paste0(r, ":", cn), paste0(r, ":", cn)])
  se
}
se <- se_clustered(m_joint, cohort$id)

make_rows <- function(term, label) {
  or_est <- exp(cf[, term])
  lo <- exp(cf[, term] - 1.96 * se[, term])
  hi <- exp(cf[, term] + 1.96 * se[, term])
  z <- cf[, term] / se[, term]
  p <- 2 * (1 - pnorm(abs(z)))
  data.frame(
    Exposure  = label,
    Direction = rownames(cf),
    OR = fmt2(or_est),
    CI = sprintf("%.2f, %.2f", lo, hi),
    p  = ifelse(p < 0.001, "<0.001", fmt2(p)),
    stringsAsFactors = FALSE
  )
}

height_rows <- make_rows("height_10cm", "Joint model, Height, per 10 cm")
weight_rows <- make_rows("weight_10kg", "Joint model, Weight, per 10 kg")

# BMI model (separate)
m_bmi <- multinom(direction ~ BMI_5, data = cohort, trace = FALSE, maxit = 1000, reltol = 1e-12, model = TRUE)
cf_b <- coef(m_bmi); se_b <- se_clustered(m_bmi, cohort$id)
z_b <- cf_b[, "BMI_5"] / se_b[, "BMI_5"]
p_b <- 2 * (1 - pnorm(abs(z_b)))
bmi_rows <- data.frame(
  Exposure  = "BMI, per 5 kg/m\u00b2",
  Direction = rownames(cf_b),
  OR = fmt2(exp(cf_b[, "BMI_5"])),
  CI = sprintf("%.2f, %.2f", exp(cf_b[, "BMI_5"] - 1.96*se_b[, "BMI_5"]),
                             exp(cf_b[, "BMI_5"] + 1.96*se_b[, "BMI_5"])),
  p  = ifelse(p_b < 0.001, "<0.001", fmt2(p_b)),
  stringsAsFactors = FALSE
)

table3_df <- rbind(height_rows, weight_rows, bmi_rows)
colnames(table3_df) <- c("Exposure", "Direction", "OR", "95% CI", "p-value")

print(table3_df)

footnote_txt <- paste(
  "OR, odds ratio; CI, confidence interval. Height and weight were estimated together in a single model;",
  "BMI was estimated in a separate model. An OR above 1 indicates higher odds of erring in that direction",
  "relative to an accurate estimate.",
  sprintf("Standard errors are cluster-robust (clustered by woman) (%s deliveries; %s women).",
          format(nrow(cohort), big.mark = ","), format(n_distinct(cohort$id), big.mark = ",")))

make_table_docx(
  table_df     = table3_df,
  caption_txt  = "Table 3. Multinomial logistic regression, reference category estimation within \u00b110%",
  footnote_txt = footnote_txt,
  path         = file.path(OUTPUT_DIR, "table3.docx")
)
cat("table3.docx written\n")

# =============================================================================
# table2_clinical.R
# Table 2. Linear regression of maternal anthropometry on signed percentage
# error, in clinical units (per 10 cm / per 10 kg / per 5 kg/m2).
# =============================================================================

source("cohort_setup.R")
library(officer)
library(flextable)

res    <- build_cohort()
cohort <- res$cohort

fmt3 <- function(x) sprintf("%.3f", x)

# All CIs / p-values use cluster-robust SEs (clustered by woman, `id`);
# point estimates and R2 are the ordinary OLS ones.
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

# Joint model (primary): height + weight together
m_joint <- lm(signed_err ~ height_10cm + weight_10kg, data = cohort)
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
  Exposure = "Joint model, Weight, per 10 kg",
  Beta = sprintf("%+.2f", cl$est["weight_10kg"]),
  CI   = sprintf("%.2f, %.2f", cl$lo["weight_10kg"], cl$hi["weight_10kg"]),
  R2   = "",
  p    = ifelse(cl$p["weight_10kg"] < 0.001, "<0.001", fmt3(cl$p["weight_10kg"])),
  stringsAsFactors = FALSE
)

bmi_row    <- fit_row("signed_err ~ BMI_5",       "BMI, per 5 kg/m\u00b2")
height_row <- fit_row("signed_err ~ height_10cm", "Alone, Height, per 10 cm")
weight_row <- fit_row("signed_err ~ weight_10kg", "Alone, Weight, per 10 kg")

table2_df <- rbind(row_h, row_w, bmi_row, height_row, weight_row)
colnames(table2_df) <- c("Exposure", "Beta", "95% CI", "R\u00b2", "p-value")

print(table2_df)

footnote_txt <- paste(
  "Beta is expressed in percentage points of signed error; negative values indicate underestimation.",
  "Height and weight were entered jointly in the primary model; BMI, height alone, and weight alone are",
  "supporting models. R\u00b2 for the joint model refers to the full two-variable model. CI, confidence interval.",
  sprintf("Standard errors are cluster-robust (clustered by woman) to account for repeat deliveries (%s deliveries; %s women).",
          format(nrow(cohort), big.mark = ","), format(n_distinct(cohort$id), big.mark = ",")))

make_table_docx(
  table_df     = table2_df,
  caption_txt  = "Table 2. Linear regression of maternal height and weight at delivery on signed percentage error",
  footnote_txt = footnote_txt,
  path         = file.path(OUTPUT_DIR, "table2.docx")
)
cat("table2.docx written\n")
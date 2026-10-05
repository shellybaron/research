# =============================================================================
# tableS1.R
# Tables S1a and S1b (supplementary). Repeat-delivery sensitivity analysis:
# repeats Table 2 (linear) and Table 3 (multinomial) on ONE randomly chosen
# delivery per woman, so that all observations are independent.
# Random choice uses a fixed seed (SEED_ONE_PER_WOMAN in cohort_setup.R),
# which is reported in Methods so the sample is exactly reproducible.
# Because each woman appears once, ordinary (non-clustered) SEs are used.
# =============================================================================

source("cohort_setup.R")
library(officer)
library(flextable)
library(nnet)

res    <- build_cohort()
cohort <- one_delivery_per_woman(res$cohort)
stopifnot(!any(duplicated(cohort$id)))
n_women <- nrow(cohort)
cat(sprintf("Sensitivity cohort: %d women (one delivery each), seed = %d\n",
            n_women, SEED_ONE_PER_WOMAN))

fmt2 <- function(x) sprintf("%.2f", x)
fmt3 <- function(x) sprintf("%.3f", x)
pfmt <- function(p, d = 3) ifelse(p < 0.001, "<0.001", sprintf(paste0("%.", d, "f"), p))

# ---------------------------- Table S1a: linear -------------------------------
fit_row <- function(formula_str, label, r2 = TRUE) {
  m  <- lm(as.formula(formula_str), data = cohort)
  cf <- summary(m)$coefficients; ci <- confint(m)
  term <- rownames(cf)[nrow(cf)]
  data.frame(Exposure = label,
             Beta = sprintf("%+.2f", cf[term, "Estimate"]),
             CI   = sprintf("%.2f, %.2f", ci[term, 1], ci[term, 2]),
             R2   = ifelse(r2, fmt3(summary(m)$r.squared), ""),
             p    = pfmt(cf[term, "Pr(>|t|)"]), stringsAsFactors = FALSE)
}
m_joint <- lm(signed_err ~ height_10cm + weight_10kg, data = cohort)
cf <- summary(m_joint)$coefficients; ci <- confint(m_joint); r2j <- summary(m_joint)$r.squared
joint_row <- function(term, label, r2) data.frame(
  Exposure = label, Beta = sprintf("%+.2f", cf[term, "Estimate"]),
  CI = sprintf("%.2f, %.2f", ci[term, 1], ci[term, 2]), R2 = r2,
  p = pfmt(cf[term, "Pr(>|t|)"]), stringsAsFactors = FALSE)

tableS1a_df <- rbind(
  joint_row("height_10cm", "Joint model, Height, per 10 cm", fmt3(r2j)),
  joint_row("weight_10kg", "Joint model, Weight, per 10 kg", ""),
  fit_row("signed_err ~ BMI_5",       "BMI, per 5 kg/m\u00b2"),
  fit_row("signed_err ~ height_10cm", "Alone, Height, per 10 cm"),
  fit_row("signed_err ~ weight_10kg", "Alone, Weight, per 10 kg")
)
colnames(tableS1a_df) <- c("Exposure", "Beta", "95% CI", "R\u00b2", "p-value")
print(tableS1a_df)

make_table_docx(
  table_df     = tableS1a_df,
  caption_txt  = sprintf("Table S1a. Linear regression of maternal height and weight at delivery on signed percentage error: one randomly selected delivery per woman (n = %s)",
                         format(n_women, big.mark = ",")),
  footnote_txt = paste(
    "Repeats Table 2 using one randomly selected delivery per woman (fixed random seed), so observations",
    "are independent. Beta is expressed in percentage points of signed error; negative values indicate",
    "underestimation. R\u00b2 for the joint model refers to the full two-variable model. CI, confidence interval."),
  path = file.path(OUTPUT_DIR, "tableS1a.docx")
)

# ------------------------- Table S1b: multinomial -----------------------------
cohort$direction <- relevel(cohort$direction, ref = "Within")
mn_rows <- function(m, term, label) {
  cf <- coef(m); se <- summary(m)$standard.errors
  z <- cf[, term] / se[, term]; p <- 2 * (1 - pnorm(abs(z)))
  data.frame(Exposure = label, Direction = rownames(cf),
             OR = fmt2(exp(cf[, term])),
             CI = sprintf("%.2f, %.2f", exp(cf[, term] - 1.96 * se[, term]),
                                        exp(cf[, term] + 1.96 * se[, term])),
             p = pfmt(p, 2), stringsAsFactors = FALSE)
}
m_j <- multinom(direction ~ height_10cm + weight_10kg, data = cohort,
                trace = FALSE, maxit = 1000, reltol = 1e-12)
m_b <- multinom(direction ~ BMI_5, data = cohort,
                trace = FALSE, maxit = 1000, reltol = 1e-12)
tableS1b_df <- rbind(mn_rows(m_j, "height_10cm", "Joint model, Height, per 10 cm"),
                    mn_rows(m_j, "weight_10kg", "Joint model, Weight, per 10 kg"),
                    mn_rows(m_b, "BMI_5",       "BMI, per 5 kg/m\u00b2"))
rownames(tableS1b_df) <- NULL
colnames(tableS1b_df) <- c("Exposure", "Direction", "OR", "95% CI", "p-value")
print(tableS1b_df)

make_table_docx(
  table_df     = tableS1b_df,
  caption_txt  = sprintf("Table S1b. Multinomial logistic regression, reference category estimation within \u00b110%%: one randomly selected delivery per woman (n = %s)",
                         format(n_women, big.mark = ",")),
  footnote_txt = paste(
    "Note: Repeats Table 3 using one randomly selected delivery per woman (fixed random seed).",
    "OR, odds ratio; CI, confidence interval. Height and weight were estimated together in a single model;",
    "BMI was estimated in a separate model."),
  path = file.path(OUTPUT_DIR, "tableS1b.docx")
)
cat("tableS1a.docx, tableS1b.docx written\n")

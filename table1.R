# =============================================================================
# table1.R
# Table 1. Maternal and neonatal characteristics by direction of clinical
# estimation error.
# Exposure: weight at delivery. BMI calculated from weight at delivery.
# =============================================================================

source("cohort_setup.R")
library(officer)
library(flextable)

res    <- build_cohort()
cohort <- res$cohort

med_iqr <- function(x) {
  q <- quantile(x, c(.25, .5, .75), na.rm = TRUE)
  sprintf("%.1f (%.1f, %.1f)", q[2], q[1], q[3])
}

n_pct <- function(x, n_total) {
  sprintf("%s (%s%%)", format(x, big.mark = ","), sprintf("%.1f", x / n_total * 100))
}

# Order columns: Overestimation, Within, Underestimation (matches manuscript order)
groups <- c("Overestimation", "Within", "Underestimation")

get_group <- function(g) cohort %>% filter(direction == g)

n_by_group <- sapply(groups, function(g) nrow(get_group(g)))

continuous_rows <- function(varname, label) {
  vals <- sapply(groups, function(g) med_iqr(get_group(g)[[varname]]))
  data.frame(Characteristic = label, t(vals), check.names = FALSE)
}

# Hebrew -> English translation for categorical raw values
translate_map <- c(
  "\u05D6\u05DB\u05E8" = "Male",              # זכר
  "\u05E0\u05E7\u05D1\u05D4" = "Female",      # נקבה
  "\u05E8\u05D2\u05D9\u05DC\u05D4" = "Vaginal",           # רגילה
  "\u05E0\u05D9\u05EA\u05D5\u05D7 \u05E7\u05D9\u05E1\u05E8\u05D9" = "Cesarean", # ניתוח קיסרי
  "\u05D5\u05D5\u05D0\u05E7\u05D5\u05DD" = "Vacuum",      # וואקום
  "\u05E2\u05DB\u05D5\u05D6" = "Breech"                    # עכוז
)

# Fixed display order for known categorical variables (clinically sensible order)
level_order <- list(
  neonatal_sex     = c("Female", "Male"),
  mode_of_delivery = c("Vaginal", "Vacuum", "Cesarean", "Breech")
)

categorical_rows <- function(varname, label) {
  sub_list <- lapply(groups, function(g) {
    v <- get_group(g)[[varname]]
    v <- ifelse(v %in% names(translate_map), translate_map[v], v)
    v
  })
  levels_all <- level_order[[varname]]
  header <- data.frame(Characteristic = label,
                        matrix("", nrow = 1, ncol = length(groups)),
                        check.names = FALSE)
  colnames(header) <- c("Characteristic", groups)
  rows <- lapply(levels_all, function(lv) {
    counts <- sapply(seq_along(groups), function(i) sum(sub_list[[i]] == lv, na.rm = TRUE))
    vals <- sapply(seq_along(groups), function(i) n_pct(counts[i], n_by_group[i]))
    r <- data.frame(Characteristic = paste0("  ", lv), t(vals), check.names = FALSE)
    colnames(r) <- c("Characteristic", groups)
    r
  })
  do.call(rbind, c(list(header), rows))
}

age_row     <- continuous_rows("age_at_birth",  "Maternal age (years)")
colnames(age_row) <- c("Characteristic", groups)
height_row  <- continuous_rows("height_cm",     "Maternal height (cm)")
colnames(height_row) <- c("Characteristic", groups)
weight_row  <- continuous_rows("weight_current","Weight at delivery (kg)")
colnames(weight_row) <- c("Characteristic", groups)
bmi_row     <- continuous_rows("BMI_delivery",  "BMI at delivery (kg/m\u00b2)")
colnames(bmi_row) <- c("Characteristic", groups)
ga_row      <- continuous_rows("gestational_week","Gestational age (weeks)")
colnames(ga_row) <- c("Characteristic", groups)
bw_row      <- continuous_rows("birth_weight",  "Birth weight (g)")
colnames(bw_row) <- c("Characteristic", groups)

sex_rows  <- categorical_rows("neonatal_sex",    "Neonatal sex")
mode_rows <- categorical_rows("mode_of_delivery","Mode of delivery")

header_n <- data.frame(
  Characteristic = "N",
  t(sprintf("%s", format(n_by_group, big.mark = ","))),
  check.names = FALSE
)
colnames(header_n) <- c("Characteristic", groups)

table1_df <- rbind(
  header_n,
  age_row,
  height_row,
  weight_row,
  bmi_row,
  ga_row,
  sex_rows,
  mode_rows,
  bw_row
)

# rename columns to include N for display (single line, avoid encoding issues)
colnames(table1_df) <- c(
  "Characteristic",
  sprintf("Overestimation (N = %s)", format(n_by_group[1], big.mark = ",")),
  sprintf("Within +/-10%% (N = %s)", format(n_by_group[2], big.mark = ",")),
  sprintf("Underestimation (N = %s)", format(n_by_group[3], big.mark = ","))
)

# Drop the redundant first "N" row now that N is in the header
table1_df <- table1_df[-1, ]

print(table1_df)

make_table_docx(
  table_df     = table1_df,
  caption_txt  = "Table 1. Maternal and neonatal characteristics by direction of clinical estimation error",
  footnote_txt = sprintf("Median (Q1, Q3); n (%%). Overestimation: >+10%%; within: \u00b110%%; underestimation: <-10%% of birth weight. BMI at delivery was calculated from weight at delivery and height. N = %s deliveries from %s women.",
                         format(nrow(cohort), big.mark = ","), format(n_distinct(cohort$id), big.mark = ",")),
  path         = file.path(OUTPUT_DIR, "table1.docx")
)
cat("table1.docx written\n")
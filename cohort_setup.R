# =============================================================================
# cohort_setup.R
# Shared cohort construction, sourced by table1.R, table2.R, table3.R, figure2.R
# Exposure variable: weight at delivery (per co-author decision — clinician only
# knows current weight at the bedside). BMI is calculated from weight at
# delivery, NOT pre-pregnancy weight.
#
# UPDATE: data file now carries a patient identifier (`id`, one per woman).
# 19,277 cohort deliveries come from 15,859 women, so repeat deliveries are
# handled by (a) cluster-robust SEs by `id` in the primary models, and
# (b) a one-random-delivery-per-woman sensitivity analysis (table6.R).
# =============================================================================

# Ensure UTF-8 locale so Hebrew source strings (neonatal_sex, mode_of_delivery)
# translate correctly; the container default locale is POSIX/C otherwise.
Sys.setlocale("LC_ALL", "C.UTF-8")

library(readxl)
library(dplyr)
library(sandwich)   # cluster-robust variance (vcovCL)
library(lmtest)      # coeftest / coefci with a supplied vcov

DATA_PATH <- "fetel_we_data_20172024_with_id.xlsx"

OUTPUT_DIR <- "output"
if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

build_cohort <- function(path = DATA_PATH) {

  raw <- read_excel(path)

  # Height column is mixed units: most values are in meters (e.g. 1.62),
  # ~1,006 records are already in cm. Normalize everything to cm.
  raw <- raw %>%
    mutate(height_cm = ifelse(height < 10, height * 100, height))

  n0 <- nrow(raw)

  # Step-by-step exclusions (mirrors Figure 1 derivation)
  step1 <- raw %>% filter(!is.na(clinical_weight_estimate))
  step2 <- step1 %>% filter(!is.na(height_cm))
  step3 <- step2 %>% filter(height_cm >= 140, height_cm <= 190)
  step4 <- step3 %>% filter(!is.na(weight_current))
  step5 <- step4 %>% filter(weight_current >= 35, weight_current <= 200)
  step6 <- step5 %>% filter(clinical_weight_estimate >= 1500,
                             clinical_weight_estimate <= 5000)

  cohort <- step6 %>%
    mutate(
      signed_err   = (clinical_weight_estimate - birth_weight) / birth_weight * 100,
      BMI_delivery = weight_current / (height_cm / 100)^2,
      height_10cm  = height_cm / 10,
      weight_10kg  = weight_current / 10,
      BMI_5        = BMI_delivery / 5,
      direction    = case_when(
        signed_err >  10 ~ "Overestimation",
        signed_err < -10 ~ "Underestimation",
        TRUE              ~ "Within"
      ),
      direction = factor(direction, levels = c("Within", "Underestimation", "Overestimation"))
    )

  # Derivation counts for Figure 1 (n at each step)
  derivation <- data.frame(
    step = c(
      "Term singleton live births in study period",
      "Missing clinical estimate",
      "Missing maternal height",
      "Implausible height (outside 140-190 cm)",
      "Missing maternal weight at delivery",
      "Implausible maternal weight (outside 35-200 kg)",
      "Implausible estimate (outside 1500-5000 g)"
    ),
    excluded = c(
      NA,
      n0 - nrow(step1),
      nrow(step1) - nrow(step2),
      nrow(step2) - nrow(step3),
      nrow(step3) - nrow(step4),
      nrow(step4) - nrow(step5),
      nrow(step5) - nrow(step6)
    ),
    remaining = c(n0, nrow(step1), nrow(step2), nrow(step3), nrow(step4), nrow(step5), nrow(step6))
  )

  list(cohort = cohort, derivation = derivation)
}


# -----------------------------------------------------------------------------
# Repeat-delivery handling
# -----------------------------------------------------------------------------
SEED_ONE_PER_WOMAN <- 20250101   # fixed + reported in Methods for reproducibility

# One randomly chosen delivery per woman (sensitivity analysis).
one_delivery_per_woman <- function(cohort, seed = SEED_ONE_PER_WOMAN) {
  set.seed(seed)
  cohort %>%
    group_by(id) %>%
    slice_sample(n = 1) %>%
    ungroup()
}


# -----------------------------------------------------------------------------
# sandwich::vcovCL() on nnet::multinom needs an estfun() method that only exists
# in recent sandwich versions. If the installed version lacks it, define the
# (standard) score and bread here so the script works either way. Models must
# be fitted with model = TRUE so the model frame is stored.
# -----------------------------------------------------------------------------
if (is.null(utils::getS3method("estfun", "multinom", optional = TRUE))) {
  estfun.multinom <- function(x, ...) {
    mf <- x$model
    if (is.null(mf)) stop("Fit multinom() with model = TRUE")
    X  <- model.matrix(x$terms, mf)
    y  <- mf[[1]]
    lv <- x$lev
    P  <- fitted(x); if (is.null(dim(P))) P <- cbind(1 - P, P)
    colnames(P) <- lv
    out <- do.call(cbind, lapply(lv[-1], function(k) {
      u <- (as.numeric(y == k) - P[, k]) * X
      colnames(u) <- paste0(k, ":", colnames(X))
      u
    }))
    out
  }
  bread.multinom <- function(x, ...) vcov(x) * NROW(x$fitted.values)
  registerS3method("estfun", "multinom", estfun.multinom, envir = asNamespace("sandwich"))
  registerS3method("bread",  "multinom", bread.multinom,  envir = asNamespace("sandwich"))
}

# Cluster-robust (by woman) coefficient table for an lm/glm/multinom fit.
# Returns estimate, SE, 95% CI and p, using sandwich::vcovCL (type HC0 for
# multinom, which has no leverage-based adjustment; HC1 for lm).
cluster_vcov <- function(model, cluster) {
  type <- if (inherits(model, "multinom")) "HC0" else "HC1"
  sandwich::vcovCL(model, cluster = cluster, type = type)
}

lm_cluster <- function(model, cluster) {
  V  <- cluster_vcov(model, cluster)
  ct <- lmtest::coeftest(model, vcov. = V)
  ci <- lmtest::coefci(model, vcov. = V)
  list(est = ct[, "Estimate"], se = ct[, "Std. Error"],
       lo = ci[, 1], hi = ci[, 2], p = ct[, "Pr(>|t|)"])
}


# -----------------------------------------------------------------------------
# Collector used by run_all.R to assemble one combined Word file. Every table /
# figure script registers its output here (harmless when a script is run alone).
# -----------------------------------------------------------------------------
if (!exists("DOC_ITEMS")) DOC_ITEMS <- list()

# Footnote paragraph: an italic "Note:" prefix, remainder in regular type.
footnote_par <- function(footnote_txt, fontname = "Times New Roman", size = 12) {
  library(officer)
  if (startsWith(footnote_txt, "Note:")) {
    fpar(ftext("Note:", fp_text(font.family = fontname, font.size = size, italic = TRUE)),
         ftext(substring(footnote_txt, 6), fp_text(font.family = fontname, font.size = size)))
  } else {
    fpar(ftext(footnote_txt, fp_text(font.family = fontname, font.size = size)))
  }
}

# Figure caption paragraph: bold label ("Figure 2.") + regular remainder.
figure_caption_par <- function(label, rest, fontname = "Times New Roman", size = 11) {
  library(officer)
  fpar(ftext(label, fp_text(font.family = fontname, bold = TRUE, font.size = size)),
       ftext(rest,  fp_text(font.family = fontname, font.size = size)))
}

# Register a figure (PNG + caption) for the combined document.
register_figure <- function(id, png, width, height, label, rest) {
  DOC_ITEMS[[id]] <<- list(type = "figure", png = png, width = width,
                           height = height, label = label, rest = rest)
}

# Helper: build a flextable from a data.frame and save as standalone Word docx.
# Handles caption/table/footnote font (Times New Roman by default) and page
# orientation in one place, so table scripts only supply their own content.
make_table_docx <- function(table_df, caption_txt, footnote_txt, path,
                            landscape = FALSE, fontname = "Times New Roman") {
  library(officer)
  library(flextable)
  
  # Split "Table 4. Rest of caption..." into a bold label + regular remainder
  m <- regexpr("^[^.]*\\.", caption_txt)
  caption_prefix <- regmatches(caption_txt, m)
  caption_rest   <- substring(caption_txt, attr(m, "match.length") + 1)
  
  ft <- flextable(table_df) %>%
    set_caption(
      caption = as_paragraph(
        as_chunk(caption_prefix, props = fp_text_default(font.family = fontname, bold = TRUE,  font.size = 12)),
        as_chunk(caption_rest,   props = fp_text_default(font.family = fontname, bold = FALSE, font.size = 12))
      )
    ) %>%
    flextable::font(fontname = fontname, part = "all") %>%
    autofit()
  
  if (landscape) {
    page_w <- 11; page_h <- 8.5
  } else {
    page_w <- 8.5; page_h <- 11
  }
  margins <- 0.5
  
  ft <- ft %>%
    flextable::fit_to_width(max_width = page_w - 2 * margins) %>%
    flextable::set_table_properties(opts_word = list(split = FALSE)) %>%
    flextable::padding(padding.top = 2, padding.bottom = 2, part = "all")
  
  doc <- read_docx()
  doc <- doc %>% body_add_flextable(ft, align = "left")
  if (!is.null(footnote_txt)) {
    doc <- doc %>% body_add_fpar(footnote_par(footnote_txt, fontname))
  }
  if (landscape) {
    doc <- doc %>% body_end_section_landscape()
  } else {
    doc <- doc %>% body_end_section_portrait()
  }
  
  print(doc, target = path)
  DOC_ITEMS[[tools::file_path_sans_ext(basename(path))]] <<- list(
    type = "table", ft = ft, footnote = footnote_txt, fontname = fontname)
  invisible(ft)
}
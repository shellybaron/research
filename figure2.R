# =============================================================================
# figure2.R
# Figure 2. Direction of clinical estimation error across bands of maternal
# anthropometry.
# Panel A: height, 10 cm bands.
# Panel B: weight at delivery, 15 kg bands.
# Panel C: four extreme-quartile corner groups (height x weight simultaneously).
#          Labels: "Short, higher weight" / "Short, lower weight" /
#                  "Tall, higher weight" / "Tall, lower weight"
#
# UPDATE (per review comment): median birth weight is shown per band in ALL
# three panels, to make explicit that taller women's babies are genuinely
# heavier while the clinical estimate rises far less (see Tables
# S3a-b).
# =============================================================================

source("cohort_setup.R")
library(ggplot2)
library(officer)

res    <- build_cohort()
cohort <- res$cohort

direction_summary <- function(df) {
  n <- nrow(df)
  under <- sum(df$signed_err < -10) / n * 100
  over  <- sum(df$signed_err >  10) / n * 100
  within <- 100 - under - over
  med   <- median(df$signed_err)
  bw_med <- median(df$birth_weight)
  est_med <- median(df$clinical_weight_estimate)
  data.frame(n = n, Underestimation = under, Within = within,
             Overestimation = over, median = med, bw_median = bw_med,
             est_median = est_med)
}

# ---- Panel A: height, 10 cm bands ------------------------------------------
cohort$hband <- cut(cohort$height_cm,
                     breaks = c(-Inf, 150, 160, 170, Inf),
                     labels = c("<150", "150-159", "160-169", "\u2265170"),
                     right = FALSE)

panelA <- do.call(rbind, lapply(levels(cohort$hband), function(lv) {
  cbind(band = lv, direction_summary(cohort[cohort$hband == lv, ]))
}))
panelA$panel <- "A. Maternal height (cm)"

# ---- Panel B: weight at delivery, 15 kg bands ------------------------------
cohort$wband <- cut(cohort$weight_current,
                     breaks = c(-Inf, 65, 80, 95, Inf),
                     labels = c("<65", "65-79", "80-94", "\u226595"),
                     right = FALSE)

panelB <- do.call(rbind, lapply(levels(cohort$wband), function(lv) {
  cbind(band = lv, direction_summary(cohort[cohort$wband == lv, ]))
}))
panelB$panel <- "B. Maternal weight at delivery (kg)"

# ---- Panel C: extreme quartile corner groups -------------------------------
h25 <- quantile(cohort$height_cm, 0.25); h75 <- quantile(cohort$height_cm, 0.75)
w25 <- quantile(cohort$weight_current, 0.25); w75 <- quantile(cohort$weight_current, 0.75)

corner <- function(h_cond, w_cond, label) {
  sub <- cohort[h_cond(cohort$height_cm) & w_cond(cohort$weight_current), ]
  cbind(band = label, direction_summary(sub))
}

panelC <- rbind(
  corner(function(h) h <= h25, function(w) w >= w75,
         sprintf("Short, higher weight\n(\u2264%.0f cm, \u2265%.0f kg)", h25, w75)),
  corner(function(h) h <= h25, function(w) w <= w25,
         sprintf("Short, lower weight\n(\u2264%.0f cm, \u2264%.0f kg)", h25, w25)),
  corner(function(h) h >= h75, function(w) w >= w75,
         sprintf("Tall, higher weight\n(\u2265%.0f cm, \u2265%.0f kg)", h75, w75)),
  corner(function(h) h >= h75, function(w) w <= w25,
         sprintf("Tall, lower weight\n(\u2265%.0f cm, \u2264%.0f kg)", h75, w25))
)
panelC$panel <- "C. Extreme quartiles of both measures"

print(panelA)
print(panelB)
print(panelC)

all_data <- rbind(panelA, panelB, panelC)
all_data$band <- factor(all_data$band, levels = rev(all_data$band))

long <- reshape(
  all_data,
  varying = c("Underestimation", "Within", "Overestimation"),
  v.names = "pct",
  timevar = "category",
  times = c("Underestimation", "Within", "Overestimation"),
  direction = "long"
)
long$category <- factor(long$category,
                         levels = c("Underestimation", "Within", "Overestimation"))

fill_colors <- c(
  "Underestimation" = "#1f6fb4",
  "Within"          = "#e5e0d8",
  "Overestimation"  = "#e07b39"
)

# In-bar percentage labels for ALL three categories, positioned explicitly at the middle of each segment so placement does
# not depend on the ggplot2 version's position_stack behaviour.
lab_df <- long %>%
  dplyr::group_by(panel, band) %>%
  dplyr::mutate(
    under_pct  = pct[category == "Underestimation"],
    within_pct = pct[category == "Within"],
    xmid = dplyr::case_when(
      category == "Underestimation" ~ pct / 2,
      category == "Within"          ~ under_pct + pct / 2,
      TRUE                          ~ under_pct + within_pct + pct / 2
    ),
    label_val = sprintf("%.1f%%", pct)
  ) %>%
  dplyr::ungroup()

n_med_df <- unique(all_data[, c("panel", "band", "n", "median", "bw_median", "est_median")])
n_med_df$n_label   <- format(n_med_df$n, big.mark = ",")
n_med_df$med_label <- sprintf("%+.2f", n_med_df$median)
n_med_df$bw_label  <- format(round(n_med_df$bw_median), big.mark = ",")
n_med_df$est_label <- format(round(n_med_df$est_median), big.mark = ",")

# Birth-weight and clinical-estimate medians are shown for all three panels,
# side by side, so the gap between true and estimated weight can be read directly.

# Header row for the right-side annotation columns, drawn once per panel.
panelA_top <- length(levels(factor(panelA$band))) + 0.65
header_A <- data.frame(
  panel = "A. Maternal height (cm)",
  x = c(109, 118, 136, 154),
  y = panelA_top,
  label = c("n", "Median\nerror", "Median\nestimate (g)", "Median\nbirth wt (g)")
)
panelB_top <- length(levels(factor(panelB$band))) + 0.65
header_B <- data.frame(
  panel = "B. Maternal weight at delivery (kg)",
  x = c(109, 118, 136, 154),
  y = panelB_top,
  label = c("n", "Median\nerror", "Median\nestimate (g)", "Median\nbirth wt (g)")
)
panelC_top <- length(levels(factor(panelC$band))) + 0.65
header_C <- data.frame(
  panel = "C. Extreme quartiles of both measures",
  x = c(109, 118, 136, 154),
  y = panelC_top,
  label = c("n", "Median\nerror", "Median\nestimate (g)", "Median\nbirth wt (g)")
)
header_df <- rbind(header_A, header_B, header_C)
header_df$hj <- ifelse(header_df$label == "n", 0.5, 0)

p <- ggplot(long, aes(x = pct, y = band, fill = category)) +
  geom_col(width = 0.7, position = position_stack(reverse = TRUE)) +
  geom_text(data = lab_df, aes(x = xmid, y = band, label = label_val, color = category),
            inherit.aes = FALSE, size = 3.2) +
  scale_color_manual(values = c("Underestimation" = "white", "Within" = "grey30",
                                "Overestimation" = "white"), guide = "none") +
  geom_text(data = n_med_df, aes(x = 109, y = band, label = n_label),
            inherit.aes = FALSE, hjust = 0.5, size = 3.2) +
  geom_text(data = n_med_df, aes(x = 118, y = band, label = med_label),
            inherit.aes = FALSE, hjust = 0, size = 3.2) +
  geom_text(data = n_med_df, aes(x = 154, y = band, label = bw_label),
            inherit.aes = FALSE, hjust = 0, size = 3.2) +
  geom_text(data = n_med_df, aes(x = 136, y = band, label = est_label),
            inherit.aes = FALSE, hjust = 0, size = 3.2) +
  geom_text(data = header_df, aes(x = x, y = y, label = label, hjust = hj),
            inherit.aes = FALSE, size = 2.6, fontface = "bold", lineheight = 0.85) +
  facet_wrap(~panel, ncol = 1, scales = "free_y") +
  scale_fill_manual(values = fill_colors, name = NULL) +
  scale_x_continuous(limits = c(0, 172), breaks = c(0, 25, 50, 75, 100),
                      expand = c(0, 0)) +
  coord_cartesian(clip = "off") +
  labs(x = "Percent of group", y = NULL) +
  theme_minimal(base_size = 11) +
  theme(
    strip.text = element_text(hjust = 0, face = "bold", size = 11),
    strip.background = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    # centre the x-axis title and the legend on the bars (0-100), not on the
    # whole plot width (which includes the number columns on the right)
    axis.title.x = element_text(hjust = 0.25),
    legend.justification = c(0.093, 0.5),   # tuned so the legend sits under the bars
    axis.text.y = element_text(size = 9),
    plot.margin = margin(t = 5, r = 10, b = 5, l = 5)
  )

ggsave(file.path(OUTPUT_DIR, "figure2.png"), p, width = 9.3, height = 10.4, dpi = 300)
cat("figure2.png written\n")

# ---- Word document with the figure and caption -----------------------------
caption_txt <- paste(
  "Bars show the percentage of deliveries in each band that were underestimated by more",
  "than 10%, estimated within \u00b110%, or overestimated by more than 10%. Group size,",
  "median signed error, median clinical estimate, and median birth weight are shown at",
  "the right of each panel.",
  "In Panel A, median birth weight rose from 3,050 g (<150 cm) to 3,445 g (\u2265170 cm),",
  "more than the median clinical estimate (3,100 g to 3,300 g). Underestimation became more",
  "frequent and overestimation less frequent with increasing height (p for trend <0.001",
  "for both), whereas weight showed the opposite, weaker pattern (Panel B; p for trend",
  "0.010 and 0.032). Panel C combines the extreme quartiles of height and weight,",
  "producing a monotonic gradient in both directions of error."
)

fontname <- "Times New Roman"

doc <- read_docx() %>%
  body_add_img(src = file.path(OUTPUT_DIR, "figure2.png"), width = 6.3, height = 7.05) %>%
  body_add_fpar(fpar(
    ftext("Figure 2. ", fp_text(font.family = fontname, bold = TRUE, font.size = 11)),
    ftext("Direction of clinical estimation error across bands of maternal anthropometry. ",
          fp_text(font.family = fontname, font.size = 11)),
    ftext(caption_txt, fp_text(font.family = fontname, font.size = 11))
  ))

print(doc, target = file.path(OUTPUT_DIR, "figure2.docx"))

register_figure("figure2", file.path(OUTPUT_DIR, "figure2.png"), 6.3, 7.05,
                "Figure 2. ",
                paste0("Direction of clinical estimation error across bands of maternal anthropometry. ", caption_txt))

cat("figure2.docx written\n")

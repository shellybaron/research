# =============================================================================
# run_all.R
# Runs every table/figure script and assembles ONE Word file with all main and
# supplementary tables and figures:
#     output/manuscript_tables_figures.docx
#
# Usage: put this file in the same folder as the other scripts and the data
# file, set the working directory to that folder, and run:  source("run_all.R")
#
# Figure 1 (cohort flow diagram) is created outside R. If "Figure_1.docx" is in
# the folder, its image is placed first, with the caption updated using the
# numbers computed from the data.
# =============================================================================

Sys.setlocale("LC_ALL", "C.UTF-8")
library(officer)
library(flextable)

DOC_ITEMS <- list()   # filled by make_table_docx() / register_figure()

# ---- 1. run all analysis scripts (each one also writes its own file) --------
scripts <- c("table1.R", "table2.R", "table3.R", "figure2.R",
             "tableS1.R", "tableS2.R", "tableS3.R", "figureS1.R")

for (s in scripts) {
  cat("\n=== Running", s, "===\n")
  source(s, local = new.env(parent = globalenv()))
}

# ---- 2. numbers for the Figure 1 caption ------------------------------------
source("cohort_setup.R")
coh        <- build_cohort()$cohort
n_deliv    <- nrow(coh)
n_women    <- dplyr::n_distinct(coh$id)
n_multi    <- sum(table(coh$id) > 1)
fig1_rest  <- sprintf("Flow diagram of cohort selection. The final analytic cohort of %s deliveries comprised %s women; %s women (%.1f%%) contributed more than one delivery.",
                      format(n_deliv, big.mark = ","), format(n_women, big.mark = ","),
                      format(n_multi, big.mark = ","), 100 * n_multi / n_women)

png_size <- function(path) {            # width/height from the PNG header
  b <- readBin(path, "raw", 24)
  c(w = sum(as.integer(b[17:20]) * 256^(3:0)), h = sum(as.integer(b[21:24]) * 256^(3:0)))
}

fig1_png <- NULL
if (file.exists("Figure_1.docx")) {
  tmp <- tempfile(); dir.create(tmp)
  unzip("Figure_1.docx", exdir = tmp)
  imgs <- list.files(file.path(tmp, "word", "media"), full.names = TRUE)
  if (length(imgs)) fig1_png <- imgs[1]
}
if (!is.null(fig1_png)) {
  d <- png_size(fig1_png); wid <- 6.3
  DOC_ITEMS[["figure1"]] <- list(type = "figure", png = fig1_png, width = wid,
                                 height = wid * d["h"] / d["w"],
                                 label = "Figure 1. ", rest = fig1_rest)
}

# ---- 3. assemble the combined document --------------------------------------
fontname <- "Times New Roman"
doc <- read_docx()
first <- TRUE

add_item <- function(doc, id, brk = TRUE) {
  it <- DOC_ITEMS[[id]]
  if (is.null(it)) { warning("Missing item: ", id); return(doc) }
  if (brk) doc <- doc %>% body_add_break()
  if (it$type == "table") {
    doc <- doc %>% body_add_flextable(it$ft, align = "left") %>%
      body_add_fpar(footnote_par(it$footnote, it$fontname))
  } else {
    doc <- doc %>% body_add_img(src = it$png, width = it$width, height = it$height) %>%
      body_add_fpar(figure_caption_par(it$label, it$rest, fontname))
  }
  doc
}

section_title <- function(doc, txt) {
  doc %>% body_add_fpar(fpar(ftext(txt, fp_text(font.family = fontname, bold = TRUE, font.size = 14))))
}

# Section titles share a page with the first item of their section.
doc <- section_title(doc, "Main tables and figures")
main_ids <- Filter(function(i) !is.null(DOC_ITEMS[[i]]),
                   c("figure1", "table1", "table2", "table3", "figure2"))
for (k in seq_along(main_ids)) doc <- add_item(doc, main_ids[k], brk = k > 1)

doc <- doc %>% body_add_break() %>% section_title("Supplementary material")
supp_ids <- c("tableS1a", "tableS1b", "tableS2", "tableS3a", "tableS3b", "figureS1")
for (k in seq_along(supp_ids)) doc <- add_item(doc, supp_ids[k], brk = k > 1)

out <- file.path(OUTPUT_DIR, "manuscript_tables_figures.docx")
print(doc, target = out)
cat("\nCombined document written to", out, "\n")

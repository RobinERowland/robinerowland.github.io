# ============================================================
# render_cv.R
# Builds the PDF CV after update_site.R has refreshed the
# generated CV sections.
# ============================================================

required_packages <- c("rmarkdown")
installed <- rownames(installed.packages())
missing <- setdiff(required_packages, installed)
if (length(missing) > 0) {
  install.packages(missing, repos = "https://cloud.r-project.org")
}

library(rmarkdown)

dir.create("files", recursive = TRUE, showWarnings = FALSE)

rmarkdown::render(
  input = "cv/cv.Rmd",
  output_file = "Rowland_CV_2026.pdf",
  output_dir = normalizePath("files", mustWork = FALSE),
  clean = TRUE,
  quiet = FALSE
)

message("CV PDF created at files/Rowland_CV_2026.pdf")

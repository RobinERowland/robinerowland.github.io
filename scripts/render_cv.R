# ============================================================
# render_cv.R
# Creates a styled PDF CV using pagedown + Chrome
# ============================================================

required_packages <- c(
  "rmarkdown",
  "pagedown",
  "readxl"
)

installed <- rownames(installed.packages())

missing <- setdiff(
  required_packages,
  installed
)

if (length(missing) > 0) {
  install.packages(
    missing,
    repos = "https://cloud.r-project.org"
  )
}

library(rmarkdown)
library(pagedown)

# ------------------------------------------------------------
# Make sure output folder exists
# ------------------------------------------------------------

dir.create(
  "files",
  recursive = TRUE,
  showWarnings = FALSE
)

# ------------------------------------------------------------
# Render the special PDF CV template to HTML
# ------------------------------------------------------------

rmarkdown::render(
  input = "cv/cv_pdf.Rmd",
  output_file = "Rowland_CV.html",
  output_dir = "files",
  quiet = FALSE
)

# ------------------------------------------------------------
# Convert HTML to PDF using Chrome
# ------------------------------------------------------------

pagedown::chrome_print(
  input = "files/Rowland_CV.html",
  output = "files/Rowland_CV.pdf"
)

# ------------------------------------------------------------
# Remove temporary HTML
# ------------------------------------------------------------

unlink(
  "files/Rowland_CV.html"
)

message(
  "PDF CV successfully created: files/Rowland_CV.pdf"
)
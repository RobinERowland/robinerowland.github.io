# ============================================================
# render_cv.R
# Build the pagedown academic CV PDF
# ============================================================

required_packages <- c(
  "pagedown",
  "rmarkdown",
  "readxl",
  "dplyr",
  "stringr",
  "knitr",
  "glue",
  "tidyr"
)

installed <- rownames(
  installed.packages()
)

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

dir.create(
  "files",
  recursive = TRUE,
  showWarnings = FALSE
)

# Render CV to paged HTML
rmarkdown::render(
  input = "cv/cv-academic.Rmd",
  output_file = "Rowland_CV.html",
  output_dir = "files",
  quiet = FALSE
)

# Convert paged HTML to PDF
pagedown::chrome_print(
  input = normalizePath(
    "files/Rowland_CV.html",
    winslash = "/"
  ),
  
  output = normalizePath(
    "files/Rowland_CV.pdf",
    winslash = "/",
    mustWork = FALSE
  )
)

# Remove temporary HTML
unlink(
  "files/Rowland_CV.html"
)

message(
  "CV successfully created: files/Rowland_CV.pdf"
)
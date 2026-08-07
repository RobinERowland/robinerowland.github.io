library(rmarkdown)
library(pagedown)

dir.create("files", recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# Read current CV Markdown
# ------------------------------------------------------------

cv_lines <- readLines(
  "_pages/cv.md",
  warn = FALSE,
  encoding = "UTF-8"
)

# ------------------------------------------------------------
# Remove Jekyll YAML header
# ------------------------------------------------------------

yaml_marks <- which(trimws(cv_lines) == "---")

if (length(yaml_marks) >= 2) {
  cv_lines <- cv_lines[-seq(yaml_marks[1], yaml_marks[2])]
}

# ------------------------------------------------------------
# Remove Jekyll/Liquid lines
# ------------------------------------------------------------

cv_lines <- cv_lines[
  !grepl("\\{%.*%\\}", cv_lines)
]

# Remove AUTO markers
cv_lines <- cv_lines[
  !grepl("<!-- AUTO:", cv_lines, fixed = TRUE)
]

# Remove website download link from PDF itself
cv_lines <- cv_lines[
  !grepl("Download CV as PDF", cv_lines, fixed = TRUE)
]

# ------------------------------------------------------------
# Read CSS
# ------------------------------------------------------------

css_lines <- readLines(
  "cv/cv.css",
  warn = FALSE,
  encoding = "UTF-8"
)

# ------------------------------------------------------------
# Build temporary R Markdown file
# CSS is embedded directly in the document
# ------------------------------------------------------------

pdf_rmd <- c(
  "---",
  "title: \"\"",
  "output:",
  "  pagedown::html_paged:",
  "    self_contained: true",
  "number_sections: false",
  "toc: false",
  "---",
  "",
  "<style>",
  css_lines,
  "</style>",
  "",
  cv_lines
)

writeLines(
  pdf_rmd,
  "cv/cv_temp.Rmd",
  useBytes = TRUE
)

# ------------------------------------------------------------
# Render HTML
# ------------------------------------------------------------

rmarkdown::render(
  input = "cv/cv_temp.Rmd",
  output_file = "Rowland_CV.html",
  output_dir = "files",
  quiet = FALSE
)

# ------------------------------------------------------------
# Convert HTML to PDF
# ------------------------------------------------------------

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

# ------------------------------------------------------------
# Clean temporary files
# ------------------------------------------------------------

unlink("cv/cv_temp.Rmd")
unlink("files/Rowland_CV.html")

message("PDF CV created: files/Rowland_CV.pdf")
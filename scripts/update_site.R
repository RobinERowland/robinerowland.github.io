# ============================================================
# update_site.R
# Reads the six Excel source files and rebuilds Academic Pages
# Markdown pages plus data files used by the CV.
# ============================================================

required_packages <- c("readxl", "dplyr", "stringr", "purrr", "readr")

installed <- rownames(installed.packages())
missing <- setdiff(required_packages, installed)
if (length(missing) > 0) {
  install.packages(missing, repos = "https://cloud.r-project.org")
}

library(readxl)
library(dplyr)
library(stringr)
library(purrr)
library(readr)

# ----------------------------
# Helpers
# ----------------------------
clean_text <- function(x) {
  x <- as.character(x)
  x <- str_replace_all(x, "\\u00A0", " ")
  x <- str_replace_all(x, "[\\t\\r\\n]+", " ")
  x <- str_squish(x)
  x[x == "NA"] <- ""
  x
}

format_date_value <- function(x, style = c("month_year", "day_month_year", "as_written")) {
  style <- match.arg(style)

  if (inherits(x, "POSIXt") || inherits(x, "Date")) {
    x <- as.Date(x)
    if (style == "month_year") return(format(x, "%b %Y"))
    if (style == "day_month_year") return(format(x, "%d %b %Y"))
  }

  out <- clean_text(x)
  out
}

escape_markdown <- function(x) {
  x <- clean_text(x)
  # Keep normal punctuation; only protect literal pipe characters.
  str_replace_all(x, "\\|", "\\\\|")
}

write_page <- function(path, title, permalink, body_lines) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  header <- c(
    "---",
    "layout: archive",
    paste0('title: "', title, '"'),
    paste0("permalink: ", permalink),
    "author_profile: true",
    "---",
    ""
  )
  writeLines(c(header, body_lines), path, useBytes = TRUE)
}

write_csv_clean <- function(df, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(df, path, na = "")
}

# ----------------------------
# Input files
# ----------------------------
input_dir <- "source-data"

files <- list(
  grants = file.path(input_dir, "Grants.xlsx"),
  invited = file.path(input_dir, "Invited_Seminars.xlsx"),
  outreach = file.path(input_dir, "Outreach.xlsx"),
  presentations = file.path(input_dir, "Presentations.xlsx"),
  science = file.path(input_dir, "Science_Communication.xlsx"),
  service = file.path(input_dir, "Service_Roles.xlsx")
)

missing_files <- names(files)[!file.exists(unlist(files))]
if (length(missing_files) > 0) {
  stop(
    "Missing source file(s): ",
    paste(unlist(files[missing_files]), collapse = ", ")
  )
}

# ----------------------------
# Read + clean source files
# ----------------------------
grants <- read_excel(files$grants) %>%
  transmute(
    Date = clean_text(Date),
    Amount_AUD = suppressWarnings(as.numeric(`Amount (AUD)`)),
    Funder = clean_text(Funder),
    Project = clean_text(Project),
    Role = clean_text(Role)
  )

invited <- read_excel(files$invited) %>%
  transmute(
    Date = map_chr(Date, format_date_value, style = "month_year"),
    Details = clean_text(`Invited Seminars`)
  )

outreach <- read_excel(files$outreach, sheet = "Outreach") %>%
  transmute(
    Date = map_chr(Date, format_date_value, style = "month_year"),
    Details = clean_text(`Talk Details`)
  )

presentations <- read_excel(files$presentations) %>%
  transmute(
    Date = map_chr(Date, format_date_value, style = "month_year"),
    Details = clean_text(`Talk Details`)
  )

science <- read_excel(files$science) %>%
  transmute(
    Date = map_chr(Date, format_date_value, style = "day_month_year"),
    Talk = clean_text(Talk),
    Link = clean_text(Link)
  )

service <- read_excel(files$service) %>%
  transmute(
    Date = clean_text(Date),
    Role = clean_text(Role)
  )

# ----------------------------
# Write cleaned CSV data
# These are useful both for debugging and for future Jekyll use.
# ----------------------------
write_csv_clean(grants, "_data/grants.csv")
write_csv_clean(invited, "_data/invited_seminars.csv")
write_csv_clean(outreach, "_data/outreach.csv")
write_csv_clean(presentations, "_data/presentations.csv")
write_csv_clean(science, "_data/science_communication.csv")
write_csv_clean(service, "_data/service_roles.csv")

# ----------------------------
# Grants page
# Preserve spreadsheet order; do NOT group by year.
# ----------------------------
grants_lines <- map_chr(seq_len(nrow(grants)), function(i) {
  row <- grants[i, ]
  amount <- ifelse(
    is.na(row$Amount_AUD),
    "",
    paste0("$", format(row$Amount_AUD, big.mark = ",", scientific = FALSE, trim = TRUE), " AUD")
  )

  pieces <- c(
    paste0("**", escape_markdown(row$Date), "**"),
    if (amount != "") paste0("**", amount, "**") else NULL,
    escape_markdown(row$Funder),
    escape_markdown(row$Project),
    if (row$Role != "") paste0("*", escape_markdown(row$Role), "*") else NULL
  )

  paste0("- ", paste(pieces, collapse = " — "))
})

write_page(
  "_pages/grants.md",
  "Grants and Awards",
  "/grants/",
  grants_lines
)

# ----------------------------
# Invited seminars page
# ----------------------------
invited_lines <- map_chr(seq_len(nrow(invited)), function(i) {
  paste0(
    "- **", escape_markdown(invited$Date[i]), "** — ",
    escape_markdown(invited$Details[i])
  )
})

write_page(
  "_pages/invited-seminars.md",
  "Invited Seminars",
  "/invited-seminars/",
  invited_lines
)

# ----------------------------
# Presentations page
# ----------------------------
presentation_lines <- map_chr(seq_len(nrow(presentations)), function(i) {
  paste0(
    "- **", escape_markdown(presentations$Date[i]), "** — ",
    escape_markdown(presentations$Details[i])
  )
})

write_page(
  "_pages/presentations.md",
  "Presentations and Posters",
  "/presentations/",
  presentation_lines
)

# ----------------------------
# Outreach page
# ----------------------------
outreach_lines <- map_chr(seq_len(nrow(outreach)), function(i) {
  paste0(
    "- **", escape_markdown(outreach$Date[i]), "** — ",
    escape_markdown(outreach$Details[i])
  )
})

write_page(
  "_pages/outreach.md",
  "Outreach",
  "/outreach/",
  outreach_lines
)

# ----------------------------
# Science communication page
# ----------------------------
science_lines <- map_chr(seq_len(nrow(science)), function(i) {
  talk <- escape_markdown(science$Talk[i])
  if (!is.na(science$Link[i]) && science$Link[i] != "") {
    talk <- paste0("[", talk, "](", science$Link[i], ")")
  }

  paste0(
    "- **", escape_markdown(science$Date[i]), "** — ",
    talk
  )
})

write_page(
  "_pages/science-communication.md",
  "Science Communication",
  "/science-communication/",
  science_lines
)

# ----------------------------
# Service roles page
# Blank Date rows in your spreadsheet are descriptions belonging
# to the role immediately above them.
# ----------------------------
service_lines <- character(0)
for (i in seq_len(nrow(service))) {
  date <- service$Date[i]
  role <- escape_markdown(service$Role[i])

  if (!is.na(date) && date != "") {
    service_lines <- c(
      service_lines,
      paste0("- **", escape_markdown(date), "** — ", role)
    )
  } else if (role != "") {
    service_lines <- c(service_lines, paste0("  ", role))
  }
}

write_page(
  "_pages/service-roles.md",
  "Service Roles",
  "/service-roles/",
  service_lines
)

# ----------------------------
# Build reusable Markdown fragments for the CV
# ----------------------------
dir.create("cv/generated", recursive = TRUE, showWarnings = FALSE)

writeLines(c("## Grants and Awards", "", grants_lines, ""),
           "cv/generated/grants.md", useBytes = TRUE)
writeLines(c("## Invited Seminars", "", invited_lines, ""),
           "cv/generated/invited_seminars.md", useBytes = TRUE)
writeLines(c("## Presentations and Posters", "", presentation_lines, ""),
           "cv/generated/presentations.md", useBytes = TRUE)
writeLines(c("## Science Communication", "", science_lines, ""),
           "cv/generated/science_communication.md", useBytes = TRUE)
writeLines(c("## Outreach", "", outreach_lines, ""),
           "cv/generated/outreach.md", useBytes = TRUE)
writeLines(c("## Service Roles", "", service_lines, ""),
           "cv/generated/service_roles.md", useBytes = TRUE)

message("Website pages and CV fragments updated successfully.")

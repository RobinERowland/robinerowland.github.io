library(readxl)
library(dplyr)

# Use exactly the same date rules as the printed CV.
source("cv/cv_date_helpers.R")

# ============================================================
# HELPER FUNCTIONS
# ============================================================

replace_section <- function(file, section, new_text) {
  
  txt <- readLines(
    file,
    warn = FALSE,
    encoding = "UTF-8"
  )
  
  start_tag <- paste0(
    "<!-- AUTO:",
    section,
    ":START -->"
  )
  
  end_tag <- paste0(
    "<!-- AUTO:",
    section,
    ":END -->"
  )
  
  start_i <- which(trimws(txt) == start_tag)
  end_i   <- which(trimws(txt) == end_tag)
  
  if (length(start_i) != 1 || length(end_i) != 1) {
    stop(
      paste(
        "Could not find exactly one",
        start_tag,
        "and",
        end_tag,
        "in",
        file
      )
    )
  }
  
  if (end_i <= start_i) {
    stop(
      paste(
        "END marker occurs before START marker in",
        file
      )
    )
  }
  
  new_lines <- c(
    txt[1:start_i],
    "",
    new_text,
    "",
    txt[end_i:length(txt)]
  )
  
  writeLines(
    new_lines,
    file,
    useBytes = TRUE
  )
}


# ============================================================
# READ CV DATA MASTER SHEET
# ============================================================

cv_data <- read_excel(
  "cv/cv_data.xlsx",
  sheet = "cv_entries",
  # Keep date and date_end as text so a year-only entry can never be
  # silently converted into a full Excel date.
  col_types = c("guess", "text", "text", rep("guess", 9))
)

if ("exclude" %in% names(cv_data)) {
  cv_data <- cv_data %>% filter(is.na(exclude) | exclude != 1)
}

outreach      <- cv_data %>% filter(type == "outreach")
presentations <- cv_data %>% filter(type == "presentations")
seminars      <- cv_data %>% filter(type == "seminars")
grants        <- cv_data %>% filter(type == "grants")
science       <- cv_data %>% filter(type == "sci_comm")
service       <- cv_data %>% filter(type == "service")


# ============================================================
# OUTREACH
# ============================================================

outreach_entries <- vapply(
  seq_len(nrow(outreach)),
  function(i) {
    date <- format_cv_date_range(outreach$date[i], outreach$date_end[i])
    details <- outreach$what[i]
    paste0("**", date, "** — ", details)
  },
  character(1)
)

replace_section("_pages/cv.md", "OUTREACH", paste(outreach_entries, collapse = "<br>\n"))


# ============================================================
# PRESENTATIONS
# ============================================================

presentation_entries <- vapply(
  seq_len(nrow(presentations)),
  function(i) {
    date <- format_cv_date_range(presentations$date[i], presentations$date_end[i])
    details <- presentations$what[i]
    where <- presentations$where[i]
    
    paste0(
      "**", date, "** — ",
      details,
      " — ",
      where
    )
  },
  character(1)
)

replace_section("_pages/presentations.md", "PRESENTATIONS", paste(presentation_entries, collapse = "\n\n"))
replace_section("_pages/cv.md", "PRESENTATIONS", paste(presentation_entries, collapse = "<br>\n"))


# ============================================================
# INVITED SEMINARS
# ============================================================

seminar_entries <- vapply(
  seq_len(nrow(seminars)),
  function(i) {
    date <- format_cv_date_range(seminars$date[i], seminars$date_end[i])
    details <- seminars$what[i]
    paste0("**", date, "** — ", details)
  },
  character(1)
)

replace_section("_pages/cv.md", "INVITED_SEMINARS", paste(seminar_entries, collapse = "<br>\n"))


# ============================================================
# GRANTS
# ============================================================

grant_entries <- vapply(
  seq_len(nrow(grants)),
  function(i) {
    date <- format_cv_date_range(grants$date[i], grants$date_end[i])
    amount <- grants$amount[i]
    funder <- grants$where[i]
    project <- grants$what[i]
    role <- grants$additional_info[i]
    
    if (!is.na(amount) && is.numeric(amount)) {
      amount <- format(amount, big.mark = ",", scientific = FALSE, trim = TRUE)
    }
    
    paste0("**", date, " — $", amount, " AUD** — **", funder, "** — *", project, "* — ", role)
  },
  character(1)
)

replace_section("_pages/cv.md", "GRANTS", paste(grant_entries, collapse = "<br>\n"))


# ============================================================
# SCIENCE COMMUNICATION
# ============================================================

science_entries <- vapply(
  seq_len(nrow(science)),
  function(i) {
    date <- format_cv_date_range(science$date[i], science$date_end[i])
    talk <- science$what[i]
    link <- science$url[i]
    
    has_link <- !is.na(link) && nzchar(trimws(as.character(link)))
    
    if (length(has_link) == 1 && has_link) {
      paste0("**", date, "** — [", talk, "](", link, ")")
    } else {
      paste0("**", date, "** — ", talk)
    }
  },
  character(1)
)

# Build the standalone Outreach website page from all three public-facing
# sections. Science Communication is intentionally shown first.
outreach_page_text <- paste(
  "## Science Communication",
  paste(science_entries, collapse = "\n\n"),
  "## Invited Seminars",
  paste(seminar_entries, collapse = "\n\n"),
  "## Outreach",
  paste(outreach_entries, collapse = "\n\n"),
  sep = "\n\n"
)

replace_section(
  "_pages/outreach.md",
  "OUTREACH",
  outreach_page_text
)

replace_section("_pages/cv.md", "SCIENCE_COMMUNICATION", paste(science_entries, collapse = "<br>\n"))


# ============================================================
# SERVICE ROLES
# ============================================================

service_entries <- vapply(
  seq_len(nrow(service)),
  function(i) {
    raw_date <- service$date[i]
    raw_end  <- service$date_end[i]
    role <- service$what[i]
    
    if (is.na(raw_date) || trimws(as.character(raw_date)) == "") {
      paste0("&nbsp;&nbsp;&nbsp;&nbsp;", role)
    } else {
      date <- format_cv_date_range(raw_date, raw_end)
      paste0("**", date, "** — ", role)
    }
  },
  character(1)
)

replace_section("_pages/cv.md", "SERVICE_ROLES", paste(service_entries, collapse = "<br>\n"))


# ============================================================
# FINISHED
# ============================================================

message("Website pages and CV updated successfully.")

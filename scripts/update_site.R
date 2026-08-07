library(readxl)
library(dplyr)

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


# Smart date formatter preserving exact input precision
format_date <- function(x) {
  if (
    length(x) == 0 ||
    is.na(x) ||
    trimws(as.character(x)) == ""
  ) {
    return("")
  }
  
  x_chr <- trimws(as.character(x))
  
  # If it's Excel numeric date
  if (is.numeric(x) || grepl("^\\d{5}$", x_chr)) {
    d <- as.Date(suppressWarnings(as.numeric(x_chr)), origin = "1899-12-30")
    if (!is.na(d)) {
      return(format(d, "%d-%b-%Y"))
    }
  }
  
  # If already formatted as Year only (e.g., 2023)
  if (grepl("^\\d{4}$", x_chr)) {
    return(x_chr)
  }
  
  # Try parsing standard formats to see input precision
  formats <- c(
    "%Y-%m-%d", "%d/%m/%Y", "%d-%m-%Y", 
    "%d-%b-%Y", "%d-%B-%Y", "%b-%Y", 
    "%B-%Y", "%b %Y", "%B %Y", "%Y-%m"
  )
  
  for (fmt in formats) {
    d <- suppressWarnings(as.Date(x_chr, format = fmt))
    if (!is.na(d)) {
      # Check if original string contained a day or just month/year or year
      if (grepl("^\\d{4}$", x_chr)) {
        return(format(d, "%Y"))
      } else if (grepl("^\\d{4}-\\d{2}$", x_chr) || grepl("^[A-Za-z]{3}-\\d{4}$", x_chr)) {
        return(format(d, "%b-%Y"))
      } else if (grepl("\\d{1,2}[-/][A-Za-z]+[-/]\\d{4}", x_chr) || grepl("\\d{4}-\\d{2}-\\d{2}", x_chr)) {
        return(format(d, "%d-%b-%Y"))
      } else {
        return(format(d, "%b-%Y"))
      }
    }
  }
  
  # Return original string if it's text like "present"
  x_chr
}

# Helper to combine start and end dates nicely
format_date_range <- function(start_val, end_val) {
  start_fmt <- format_date(start_val)
  end_fmt <- format_date(end_val)
  
  if (end_fmt == "" || is.na(end_val) || trimws(as.character(end_val)) == "") {
    return(start_fmt)
  } else {
    return(paste0(start_fmt, " – ", end_fmt))
  }
}


# ============================================================
# READ CV DATA MASTER SHEET
# ============================================================

cv_data <- read_excel("cv/cv_data.xlsx", sheet = "cv_entries")

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
    date <- format_date_range(outreach$date[i], outreach$date_end[i])
    details <- outreach$what[i]
    paste0("**", date, "** — ", details)
  },
  character(1)
)

replace_section("_pages/outreach.md", "OUTREACH", paste(outreach_entries, collapse = "\n\n"))
replace_section("_pages/cv.md", "OUTREACH", paste(outreach_entries, collapse = "<br>\n"))


# ============================================================
# PRESENTATIONS
# ============================================================

presentation_entries <- vapply(
  seq_len(nrow(presentations)),
  function(i) {
    date <- format_date_range(presentations$date[i], presentations$date_end[i])
    details <- presentations$what[i]
    paste0("**", date, "** — ", details)
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
    date <- format_date_range(seminars$date[i], seminars$date_end[i])
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
    date <- format_date_range(grants$date[i], grants$date_end[i])
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
    date <- format_date_range(science$date[i], science$date_end[i])
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
      date <- format_date_range(raw_date, raw_end)
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
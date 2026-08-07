library(readxl)

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


format_date <- function(x) {
  
  if (
    length(x) == 0 ||
    is.na(x) ||
    trimws(as.character(x)) == ""
  ) {
    return("")
  }
  
  if (inherits(x, "Date")) {
    return(format(x, "%Y-%b"))
  }
  
  if (inherits(x, c("POSIXct", "POSIXt"))) {
    return(format(x, "%Y-%b"))
  }
  
  if (is.numeric(x)) {
    d <- as.Date(
      x,
      origin = "1899-12-30"
    )
    
    return(format(d, "%Y-%b"))
  }
  
  x_chr <- trimws(as.character(x))
  
  # Already formatted correctly
  if (grepl(
    "^\\d{4}-[A-Za-z]{3}$",
    x_chr
  )) {
    return(x_chr)
  }
  
  formats <- c(
    "%Y-%m-%d",
    "%d/%m/%Y",
    "%d-%m-%Y",
    "%d-%b-%Y",
    "%d-%B-%Y",
    "%b-%Y",
    "%B-%Y",
    "%b %Y",
    "%B %Y",
    "%Y-%m"
  )
  
  for (fmt in formats) {
    
    d <- suppressWarnings(
      as.Date(
        x_chr,
        format = fmt
      )
    )
    
    if (!is.na(d)) {
      return(format(d, "%Y-%b"))
    }
  }
  
  # Leave ranges / Present values unchanged
  # e.g. 2025-Present, 2024-2026
  x_chr
}


# ============================================================
# READ EXCEL FILES
# ============================================================

grants <- read_excel(
  "source-data/Grants.xlsx"
)

seminars <- read_excel(
  "source-data/Invited_Seminars.xlsx"
)

outreach <- read_excel(
  "source-data/Outreach.xlsx"
)

presentations <- read_excel(
  "source-data/Presentations.xlsx"
)

science <- read_excel(
  "source-data/Science_Communication.xlsx"
)

service <- read_excel(
  "source-data/Service_Roles.xlsx"
)


# ============================================================
# OUTREACH
# ============================================================

outreach_entries <- vapply(
  seq_len(nrow(outreach)),
  function(i) {
    
    date <- format_date(
      outreach$Date[i]
    )
    
    details <- outreach[["Talk Details"]][i]
    
    paste0(
      "**",
      date,
      "** — ",
      details
    )
  },
  character(1)
)

# Standalone page:
# blank line between entries
outreach_page_text <- paste(
  outreach_entries,
  collapse = "\n\n"
)

# CV:
# new line, no paragraph gap
outreach_cv_text <- paste(
  outreach_entries,
  collapse = "<br>\n"
)

replace_section(
  "_pages/outreach.md",
  "OUTREACH",
  outreach_page_text
)

replace_section(
  "_pages/cv.md",
  "OUTREACH",
  outreach_cv_text
)


# ============================================================
# PRESENTATIONS
# ============================================================

presentation_entries <- vapply(
  seq_len(nrow(presentations)),
  function(i) {
    
    date <- format_date(
      presentations$Date[i]
    )
    
    details <- presentations[["Talk Details"]][i]
    
    paste0(
      "**",
      date,
      "** — ",
      details
    )
  },
  character(1)
)

# Standalone page
presentations_page_text <- paste(
  presentation_entries,
  collapse = "\n\n"
)

# CV
presentations_cv_text <- paste(
  presentation_entries,
  collapse = "<br>\n"
)

replace_section(
  "_pages/presentations.md",
  "PRESENTATIONS",
  presentations_page_text
)

replace_section(
  "_pages/cv.md",
  "PRESENTATIONS",
  presentations_cv_text
)


# ============================================================
# INVITED SEMINARS
# CV ONLY
# ============================================================

seminar_entries <- vapply(
  seq_len(nrow(seminars)),
  function(i) {
    
    date <- format_date(
      seminars$Date[i]
    )
    
    details <- seminars[["Invited Seminars"]][i]
    
    paste0(
      "**",
      date,
      "** — ",
      details
    )
  },
  character(1)
)

seminar_cv_text <- paste(
  seminar_entries,
  collapse = "<br>\n"
)

replace_section(
  "_pages/cv.md",
  "INVITED_SEMINARS",
  seminar_cv_text
)


# ============================================================
# GRANTS
# CV ONLY
# ============================================================

grant_entries <- vapply(
  seq_len(nrow(grants)),
  function(i) {
    
    date <- format_date(
      grants$Date[i]
    )
    
    amount <- grants[["Amount (AUD)"]][i]
    funder <- grants$Funder[i]
    project <- grants$Project[i]
    role <- grants$Role[i]
    
    if (
      !is.na(amount) &&
      is.numeric(amount)
    ) {
      amount <- format(
        amount,
        big.mark = ",",
        scientific = FALSE,
        trim = TRUE
      )
    }
    
    paste0(
      "**",
      date,
      " — $",
      amount,
      " AUD** — ",
      "**",
      funder,
      "** — *",
      project,
      "* — ",
      role
    )
  },
  character(1)
)

grant_cv_text <- paste(
  grant_entries,
  collapse = "<br>\n"
)

replace_section(
  "_pages/cv.md",
  "GRANTS",
  grant_cv_text
)


# ============================================================
# SCIENCE COMMUNICATION
# CV ONLY
# ============================================================

science_entries <- vapply(
  seq_len(nrow(science)),
  function(i) {
    
    date <- format_date(
      science$Date[i]
    )
    
    talk <- science$Talk[i]
    link <- science$Link[i]
    
    if (
      !is.na(link) &&
      nzchar(trimws(as.character(link)))
    ) {
      
      paste0(
        "**",
        date,
        "** — [",
        talk,
        "](",
        link,
        ")"
      )
      
    } else {
      
      paste0(
        "**",
        date,
        "** — ",
        talk
      )
    }
  },
  character(1)
)

science_cv_text <- paste(
  science_entries,
  collapse = "<br>\n"
)

replace_section(
  "_pages/cv.md",
  "SCIENCE_COMMUNICATION",
  science_cv_text
)


# ============================================================
# SERVICE ROLES
# CV ONLY
# ============================================================

service_entries <- vapply(
  seq_len(nrow(service)),
  function(i) {
    
    raw_date <- service$Date[i]
    role <- service$Role[i]
    
    # Blank-date rows are descriptions of
    # the previous service role
    if (
      is.na(raw_date) ||
      trimws(as.character(raw_date)) == ""
    ) {
      
      paste0(
        "&nbsp;&nbsp;&nbsp;&nbsp;",
        role
      )
      
    } else {
      
      date <- format_date(
        raw_date
      )
      
      paste0(
        "**",
        date,
        "** — ",
        role
      )
    }
  },
  character(1)
)

service_cv_text <- paste(
  service_entries,
  collapse = "<br>\n"
)

replace_section(
  "_pages/cv.md",
  "SERVICE_ROLES",
  service_cv_text
)


# ============================================================
# FINISHED
# ============================================================

message(
  "Website pages and CV updated successfully."
)
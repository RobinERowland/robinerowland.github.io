library(readxl)
library(dplyr)

# Use the same date rules as the printed CV
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


clean_entry_value <- function(x) {
  
  if (
    length(x) == 0 ||
    is.na(x) ||
    trimws(as.character(x)) == ""
  ) {
    return("")
  }
  
  trimws(as.character(x))
}


get_entry_value <- function(data, column, i) {
  
  if (!column %in% names(data)) {
    return("")
  }
  
  clean_entry_value(
    data[[column]][i]
  )
}


# Used by presentations, science communication,
# invited seminars and outreach
format_full_entry <- function(data, i) {
  
  date <- clean_entry_value(
    format_cv_date_range(
      data$date[i],
      data$date_end[i]
    )
  )
  
  title <- get_entry_value(
    data,
    "what",
    i
  )
  
  link <- get_entry_value(
    data,
    "url",
    i
  )
  
  # Make the title clickable when a URL is available
  if (nzchar(title)) {
    
    if (nzchar(link)) {
      title <- paste0(
        "[",
        title,
        "](",
        link,
        ")"
      )
    }
    
    title <- paste0(
      "**",
      title,
      "**"
    )
  }
  
  # Include every populated descriptive field
  information <- c(
    title,
    get_entry_value(data, "format", i),
    get_entry_value(data, "where", i),
    get_entry_value(data, "department", i),
    get_entry_value(data, "institution", i),
    get_entry_value(data, "additional_info", i)
  )
  
  information <- information[
    nzchar(information)
  ]
  
  entry_parts <- character(0)
  
  if (nzchar(date)) {
    entry_parts <- c(
      entry_parts,
      paste0(
        "**",
        date,
        "**"
      )
    )
  }
  
  entry_parts <- c(
    entry_parts,
    information
  )
  
  paste(
    entry_parts,
    collapse = " — "
  )
}


# ============================================================
# READ CV DATA MASTER SHEET
# ============================================================

cv_data <- read_excel(
  "cv/cv_data.xlsx",
  sheet = "cv_entries",
  
  # Keep date and date_end as text so year-only
  # entries remain year-only
  col_types = c(
    "guess",
    "text",
    "text",
    rep("guess", 9)
  )
)


# Exclude rows marked with 1
if ("exclude" %in% names(cv_data)) {
  
  cv_data <- cv_data %>%
    filter(
      is.na(exclude) |
        exclude != 1
    )
}


outreach <- cv_data %>%
  filter(type == "outreach")

presentations <- cv_data %>%
  filter(type == "presentations")

seminars <- cv_data %>%
  filter(type == "seminars")

grants <- cv_data %>%
  filter(type == "grants")

science <- cv_data %>%
  filter(type == "sci_comm")

service <- cv_data %>%
  filter(type == "service")


# ============================================================
# OUTREACH
# ============================================================

outreach_entries <- vapply(
  seq_len(nrow(outreach)),
  function(i) {
    
    format_full_entry(
      outreach,
      i
    )
  },
  character(1)
)


replace_section(
  "_pages/cv.md",
  "OUTREACH",
  paste(
    outreach_entries,
    collapse = "<br>\n"
  )
)


# ============================================================
# PRESENTATIONS
# ============================================================

presentation_entries <- vapply(
  seq_len(nrow(presentations)),
  function(i) {
    
    format_full_entry(
      presentations,
      i
    )
  },
  character(1)
)


replace_section(
  "_pages/presentations.md",
  "PRESENTATIONS",
  paste(
    presentation_entries,
    collapse = "\n\n"
  )
)


replace_section(
  "_pages/cv.md",
  "PRESENTATIONS",
  paste(
    presentation_entries,
    collapse = "<br>\n"
  )
)

# ============================================================
# INVITED SEMINARS
# ============================================================

seminar_entries <- vapply(
  seq_len(nrow(seminars)),
  function(i) {
    
    format_full_entry(
      seminars,
      i
    )
  },
  character(1)
)


replace_section(
  "_pages/cv.md",
  "INVITED_SEMINARS",
  paste(
    seminar_entries,
    collapse = "<br>\n"
  )
)


# ============================================================
# GRANTS AND AWARDS
# ============================================================

grant_entries <- vapply(
  seq_len(nrow(grants)),
  function(i) {
    
    date <- clean_entry_value(
      format_cv_date_range(
        grants$date[i],
        grants$date_end[i]
      )
    )
    
    amount_raw <- grants$amount[i]
    
    amount <- get_entry_value(
      grants,
      "amount",
      i
    )
    
    funder <- get_entry_value(
      grants,
      "where",
      i
    )
    
    project <- get_entry_value(
      grants,
      "what",
      i
    )
    
    role <- get_entry_value(
      grants,
      "additional_info",
      i
    )
    
    # Add thousands separators
    amount_numeric <- suppressWarnings(
      as.numeric(amount_raw)
    )
    
    if (
      nzchar(amount) &&
      !is.na(amount_numeric)
    ) {
      
      amount <- format(
        amount_numeric,
        big.mark = ",",
        scientific = FALSE,
        trim = TRUE
      )
    }
    
    date_amount <- character(0)
    
    if (nzchar(date)) {
      date_amount <- c(
        date_amount,
        date
      )
    }
    
    if (nzchar(amount)) {
      date_amount <- c(
        date_amount,
        paste0(
          "$",
          amount,
          " AUD"
        )
      )
    }
    
    entry_parts <- character(0)
    
    if (length(date_amount) > 0) {
      entry_parts <- c(
        entry_parts,
        paste0(
          "**",
          paste(
            date_amount,
            collapse = " — "
          ),
          "**"
        )
      )
    }
    
    if (nzchar(funder)) {
      entry_parts <- c(
        entry_parts,
        paste0(
          "**",
          funder,
          "**"
        )
      )
    }
    
    if (nzchar(project)) {
      entry_parts <- c(
        entry_parts,
        paste0(
          "*",
          project,
          "*"
        )
      )
    }
    
    if (nzchar(role)) {
      entry_parts <- c(
        entry_parts,
        role
      )
    }
    
    paste(
      entry_parts,
      collapse = " — "
    )
  },
  character(1)
)


replace_section(
  "_pages/cv.md",
  "GRANTS",
  paste(
    grant_entries,
    collapse = "<br>\n"
  )
)


# ============================================================
# SCIENCE COMMUNICATION
# ============================================================

science_entries <- vapply(
  seq_len(nrow(science)),
  function(i) {
    
    format_full_entry(
      science,
      i
    )
  },
  character(1)
)


# ============================================================
# BUILD THE STANDALONE OUTREACH PAGE
# ============================================================

outreach_page_text <- paste(
  "## Science Communication",
  paste(
    science_entries,
    collapse = "\n\n"
  ),
  
  "## Invited Seminars",
  paste(
    seminar_entries,
    collapse = "\n\n"
  ),
  
  "## Outreach",
  paste(
    outreach_entries,
    collapse = "\n\n"
  ),
  
  sep = "\n\n"
)


replace_section(
  "_pages/outreach.md",
  "OUTREACH",
  outreach_page_text
)


replace_section(
  "_pages/cv.md",
  "SCIENCE_COMMUNICATION",
  paste(
    science_entries,
    collapse = "<br>\n"
  )
)


# ============================================================
# SERVICE ROLES
# ============================================================

service_entries <- vapply(
  seq_len(nrow(service)),
  function(i) {
    
    raw_date <- service$date[i]
    raw_end  <- service$date_end[i]
    
    role <- get_entry_value(
      service,
      "what",
      i
    )
    
    # Rows without a date are descriptions
    # of the previous service role
    if (clean_entry_value(raw_date) == "") {
      
      paste0(
        "&nbsp;&nbsp;&nbsp;&nbsp;",
        role
      )
      
    } else {
      
      date <- format_cv_date_range(
        raw_date,
        raw_end
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


replace_section(
  "_pages/cv.md",
  "SERVICE_ROLES",
  paste(
    service_entries,
    collapse = "<br>\n"
  )
)


# ============================================================
# FINISHED
# ============================================================

message(
  "Website pages and CV updated successfully.")

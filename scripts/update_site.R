library(readxl)

# ------------------------------------------------------------
# Helper functions
# ------------------------------------------------------------

replace_section <- function(file, section, new_text) {
  
  txt <- readLines(file, warn = FALSE, encoding = "UTF-8")
  
  start_tag <- paste0("<!-- AUTO:", section, ":START -->")
  end_tag   <- paste0("<!-- AUTO:", section, ":END -->")
  
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
  
  new_lines <- c(
    txt[1:start_i],
    "",
    new_text,
    "",
    txt[end_i:length(txt)]
  )
  
  writeLines(new_lines, file, useBytes = TRUE)
}


format_date <- function(x) {
  
  if (inherits(x, "Date")) {
    return(format(x, "%b %Y"))
  }
  
  as.character(x)
}


# ------------------------------------------------------------
# Read Excel files
# ------------------------------------------------------------

grants <- read_excel("source-data/Grants.xlsx")

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


# ------------------------------------------------------------
# Outreach
# ------------------------------------------------------------

outreach_text <- apply(outreach, 1, function(x) {
  
  date <- format_date(x[["Date"]])
  details <- x[["Talk Details"]]
  
  paste0(
    "**", date, "** — ",
    details
  )
})

outreach_text <- paste(
  outreach_text,
  collapse = "\n\n"
)

replace_section(
  "_pages/outreach.md",
  "OUTREACH",
  outreach_text
)

replace_section(
  "_pages/cv.md",
  "OUTREACH",
  outreach_text
)


# ------------------------------------------------------------
# Presentations
# ------------------------------------------------------------

presentations_text <- apply(
  presentations,
  1,
  function(x) {
    
    date <- format_date(x[["Date"]])
    details <- x[["Talk Details"]]
    
    paste0(
      "**", date, "** — ",
      details
    )
  }
)

presentations_text <- paste(
  presentations_text,
  collapse = "\n\n"
)

replace_section(
  "_pages/presentations.md",
  "PRESENTATIONS",
  presentations_text
)

replace_section(
  "_pages/cv.md",
  "PRESENTATIONS",
  presentations_text
)


# ------------------------------------------------------------
# Invited seminars
# ------------------------------------------------------------

seminar_text <- apply(
  seminars,
  1,
  function(x) {
    
    date <- format_date(x[["Date"]])
    details <- x[["Invited Seminars"]]
    
    paste0(
      "**", date, "** — ",
      details
    )
  }
)

seminar_text <- paste(
  seminar_text,
  collapse = "\n\n"
)

replace_section(
  "_pages/cv.md",
  "INVITED_SEMINARS",
  seminar_text
)


# ------------------------------------------------------------
# Grants
# ------------------------------------------------------------

grant_text <- apply(
  grants,
  1,
  function(x) {
    
    date <- x[["Date"]]
    amount <- x[["Amount (AUD)"]]
    funder <- x[["Funder"]]
    project <- x[["Project"]]
    role <- x[["Role"]]
    
    paste0(
      "**", date, " — $",
      amount, " AUD**  \n",
      "**", funder, "** — *",
      project, "*  \n",
      role
    )
  }
)

grant_text <- paste(
  grant_text,
  collapse = "\n\n"
)

replace_section(
  "_pages/cv.md",
  "GRANTS",
  grant_text
)


# ------------------------------------------------------------
# Science communication
# ------------------------------------------------------------

science_text <- apply(
  science,
  1,
  function(x) {
    
    date <- format_date(x[["Date"]])
    talk <- x[["Talk"]]
    link <- x[["Link"]]
    
    if (!is.na(link) && nzchar(link)) {
      
      paste0(
        "**", date, "** — [",
        talk,
        "](",
        link,
        ")"
      )
      
    } else {
      
      paste0(
        "**", date, "** — ",
        talk
      )
    }
  }
)

science_text <- paste(
  science_text,
  collapse = "\n\n"
)

replace_section(
  "_pages/cv.md",
  "SCIENCE_COMMUNICATION",
  science_text
)


# ------------------------------------------------------------
# Service roles
# ------------------------------------------------------------

service_text <- apply(
  service,
  1,
  function(x) {
    
    date <- x[["Date"]]
    role <- x[["Role"]]
    
    if (is.na(date) || date == "") {
      role
    } else {
      paste0(
        "**", date, "** — ",
        role
      )
    }
  }
)

service_text <- paste(
  service_text,
  collapse = "\n\n"
)

replace_section(
  "_pages/cv.md",
  "SERVICE_ROLES",
  service_text
)

message("Website pages updated successfully.")
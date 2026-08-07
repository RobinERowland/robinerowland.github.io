# ============================================================
# render_typst_cv.R
# Updates the Typst CV from the six Excel files and compiles PDF.
#
# Run from the ROOT of the GitHub repository:
#   source("scripts/render_typst_cv.R")
#
# Required R package:
#   readxl
#
# Typst CLI must also be installed and available on PATH.
# ============================================================

if (!requireNamespace("readxl", quietly = TRUE)) {
  install.packages("readxl", repos = "https://cloud.r-project.org")
}

library(readxl)

# ------------------------------------------------------------
# Paths
# ------------------------------------------------------------

typ_file <- "cv/Rowland_CV.typ"
pdf_file <- "files/Rowland_CV.pdf"

if (!file.exists(typ_file)) {
  stop("Could not find ", typ_file)
}

dir.create("files", recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# Helper: escape text for Typst markup content
# ------------------------------------------------------------

escape_typst <- function(x) {

  if (length(x) == 0 || is.na(x)) {
    return("")
  }

  x <- trimws(as.character(x))

  # Backslash first
  x <- gsub("\\\\", "\\\\\\\\", x)

  # Escape Typst markup characters that may occur in spreadsheet text
  x <- gsub("([#\\[\\]\\$@])", "\\\\\\1", x, perl = TRUE)

  # Normalize whitespace/newlines
  x <- gsub("[\r\n\t]+", " ", x)
  x <- gsub("[[:space:]]+", " ", x)

  trimws(x)
}


# ------------------------------------------------------------
# Helper: escape a URL used inside a Typst string
# ------------------------------------------------------------

escape_typst_string <- function(x) {

  if (length(x) == 0 || is.na(x)) {
    return("")
  }

  x <- trimws(as.character(x))
  x <- gsub("\\\\", "\\\\\\\\", x)
  x <- gsub("\"", "\\\\\"", x)

  x
}


# ------------------------------------------------------------
# Date format
#
# Actual Excel dates -> 2024-Aug
# Year-only/ranges such as 2026, 2025-Present, 2024-2026 are
# preserved because no month is available.
# ------------------------------------------------------------

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
    return(format(as.Date(x), "%Y-%b"))
  }

  # readxl sometimes returns Excel dates as numeric values.
  # Values that look like calendar years are kept as years.
  if (is.numeric(x)) {

    if (x >= 1900 && x <= 2200 && x == floor(x)) {
      return(as.character(as.integer(x)))
    }

    d <- as.Date(x, origin = "1899-12-30")
    return(format(d, "%Y-%b"))
  }

  x_chr <- trimws(as.character(x))

  # Already YYYY-Mon
  if (grepl("^\\d{4}-[A-Za-z]{3}$", x_chr)) {
    return(x_chr)
  }

  # Preserve year, ranges and Present entries
  if (
    grepl("^\\d{4}$", x_chr) ||
    grepl("^\\d{4}\\s*[-–]\\s*\\d{4}$", x_chr) ||
    grepl("^\\d{4}\\s*[-–]\\s*[Pp]resent$", x_chr)
  ) {
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
      as.Date(x_chr, format = fmt)
    )

    if (!is.na(d)) {
      return(format(d, "%Y-%b"))
    }
  }

  x_chr
}


# ------------------------------------------------------------
# Helper: replace an automatically generated Typst section
# ------------------------------------------------------------

replace_typst_section <- function(file, section, new_text) {

  txt <- readLines(
    file,
    warn = FALSE,
    encoding = "UTF-8"
  )

  start_tag <- paste0("// AUTO:", section, ":START")
  end_tag   <- paste0("// AUTO:", section, ":END")

  start_i <- which(trimws(txt) == start_tag)
  end_i   <- which(trimws(txt) == end_tag)

  if (length(start_i) != 1 || length(end_i) != 1) {
    stop(
      "Could not find exactly one ",
      start_tag,
      " and ",
      end_tag,
      " in ",
      file
    )
  }

  if (end_i <= start_i) {
    stop("END marker occurs before START marker for ", section)
  }

  new_lines <- c(
    txt[seq_len(start_i)],
    "",
    new_text,
    "",
    txt[end_i:length(txt)]
  )

  writeLines(new_lines, file, useBytes = TRUE)
}


# ------------------------------------------------------------
# Helper: build a RenderCV regular entry
# ------------------------------------------------------------

regular_entry <- function(main, date = "", second_row = character()) {

  main <- escape_typst(main)
  date <- escape_typst(date)

  if (length(second_row) == 0) {
    second <- ""
  } else {
    second_row <- second_row[
      !is.na(second_row) &
      nzchar(trimws(as.character(second_row)))
    ]

    if (length(second_row) == 0) {
      second <- ""
    } else {
      bullets <- paste0(
        "    - ",
        vapply(second_row, escape_typst, character(1)),
        collapse = "\n\n"
      )

      second <- paste0("\n", bullets, "\n")
    }
  }

  paste0(
    "#regular-entry(\n",
    "  [\n",
    "    #strong[", main, "]\n",
    "  ],\n",
    "  [\n",
    "    ", date, "\n",
    "  ],\n",
    "  main-column-second-row: [",
    second,
    "  ],\n",
    ")"
  )
}


# ------------------------------------------------------------
# Read source spreadsheets
# ------------------------------------------------------------

grants <- read_excel("source-data/Grants.xlsx")
seminars <- read_excel("source-data/Invited_Seminars.xlsx")
outreach <- read_excel("source-data/Outreach.xlsx")
presentations <- read_excel("source-data/Presentations.xlsx")
science <- read_excel("source-data/Science_Communication.xlsx")
service <- read_excel("source-data/Service_Roles.xlsx")


# ============================================================
# SERVICE ROLES
# ============================================================

service_blocks <- character()

i <- 1

while (i <= nrow(service)) {

  raw_date <- service$Date[i]
  role <- service$Role[i]

  # Skip orphaned description rows safely
  if (
    is.na(raw_date) ||
    trimws(as.character(raw_date)) == ""
  ) {
    i <- i + 1
    next
  }

  descriptions <- character()
  j <- i + 1

  while (
    j <= nrow(service) &&
    (
      is.na(service$Date[j]) ||
      trimws(as.character(service$Date[j])) == ""
    )
  ) {

    descriptions <- c(
      descriptions,
      service$Role[j]
    )

    j <- j + 1
  }

  service_blocks <- c(
    service_blocks,
    regular_entry(
      main = role,
      date = format_date(raw_date),
      second_row = descriptions
    )
  )

  i <- j
}

replace_typst_section(
  typ_file,
  "SERVICE_ROLES",
  paste(service_blocks, collapse = "\n\n")
)


# ============================================================
# GRANTS
# ============================================================

grant_blocks <- vapply(
  seq_len(nrow(grants)),
  function(i) {

    amount <- grants[["Amount (AUD)"]][i]

    if (!is.na(amount) && is.numeric(amount)) {
      amount <- format(
        amount,
        big.mark = ",",
        scientific = FALSE,
        trim = TRUE
      )
    }

    main <- paste0(
      trimws(as.character(grants$Funder[i])),
      " — ",
      trimws(as.character(grants$Project[i]))
    )

    summary <- paste0(
      "$",
      amount,
      " AUD | ",
      grants$Role[i]
    )

    regular_entry(
      main = main,
      date = format_date(grants$Date[i]),
      second_row = summary
    )
  },
  character(1)
)

replace_typst_section(
  typ_file,
  "GRANTS",
  paste(grant_blocks, collapse = "\n\n")
)


# ============================================================
# INVITED SEMINARS
# ============================================================

seminar_blocks <- vapply(
  seq_len(nrow(seminars)),
  function(i) {

    regular_entry(
      main = seminars[["Invited Seminars"]][i],
      date = format_date(seminars$Date[i])
    )
  },
  character(1)
)

replace_typst_section(
  typ_file,
  "INVITED_SEMINARS",
  paste(seminar_blocks, collapse = "\n\n")
)


# ============================================================
# PRESENTATIONS
# ============================================================

presentation_blocks <- vapply(
  seq_len(nrow(presentations)),
  function(i) {

    regular_entry(
      main = presentations[["Talk Details"]][i],
      date = format_date(presentations$Date[i])
    )
  },
  character(1)
)

replace_typst_section(
  typ_file,
  "PRESENTATIONS",
  paste(presentation_blocks, collapse = "\n\n")
)


# ============================================================
# SCIENCE COMMUNICATION
# ============================================================

science_blocks <- vapply(
  seq_len(nrow(science)),
  function(i) {

    date <- format_date(science$Date[i])
    talk <- science$Talk[i]
    link <- science$Link[i]

    if (
      !is.na(link) &&
      nzchar(trimws(as.character(link)))
    ) {

      talk_escaped <- escape_typst(talk)
      url_escaped <- escape_typst_string(link)

      paste0(
        "#regular-entry(\n",
        "  [\n",
        "    #link(\"", url_escaped, "\")[#strong[",
        talk_escaped,
        "]]\n",
        "  ],\n",
        "  [\n",
        "    ", escape_typst(date), "\n",
        "  ],\n",
        "  main-column-second-row: [\n",
        "  ],\n",
        ")"
      )

    } else {

      regular_entry(
        main = talk,
        date = date
      )
    }
  },
  character(1)
)

replace_typst_section(
  typ_file,
  "SCIENCE_COMMUNICATION",
  paste(science_blocks, collapse = "\n\n")
)


# ============================================================
# OUTREACH
# ============================================================

outreach_blocks <- vapply(
  seq_len(nrow(outreach)),
  function(i) {

    regular_entry(
      main = outreach[["Talk Details"]][i],
      date = format_date(outreach$Date[i])
    )
  },
  character(1)
)

replace_typst_section(
  typ_file,
  "OUTREACH",
  paste(outreach_blocks, collapse = "\n\n")
)


# ============================================================
# COMPILE PDF
# ============================================================

typst_path <- Sys.which("typst")

if (typst_path == "") {
  stop(
    paste0(
      "\nTypst is not installed or is not on PATH.\n",
      "Install the Typst CLI, restart R/RStudio, then rerun this script.\n",
      "After installation, this should return a path:\n",
      "  Sys.which(\"typst\")\n"
    )
  )
}

status <- system2(
  typst_path,
  args = c(
    "compile",
    shQuote(typ_file),
    shQuote(pdf_file)
  )
)

if (!identical(status, 0L)) {
  stop("Typst compilation failed.")
}

message(
  "Typst CV updated and PDF created successfully: ",
  pdf_file
)

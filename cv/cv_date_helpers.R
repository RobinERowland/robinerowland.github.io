# ============================================================
# Shared CV date helpers
# Used by both update_site.R and cv-academic.Rmd
# ============================================================

is_blank_cv_date <- function(x) {
  length(x) == 0 ||
    is.na(x) ||
    trimws(as.character(x)) == ""
}


parse_cv_date_one <- function(x) {

  if (is_blank_cv_date(x)) {
    return(as.Date(NA))
  }

  if (inherits(x, "Date")) {
    return(x)
  }

  if (inherits(x, c("POSIXct", "POSIXt"))) {
    return(as.Date(x))
  }

  x_chr <- trimws(as.character(x))

  # A four-digit number is a YEAR, never an Excel serial date.
  if (grepl("^\\d{4}$", x_chr)) {
    return(as.Date(paste0(x_chr, "-01-01")))
  }

  # Open-ended ranges such as 2023 - present.
  if (tolower(x_chr) %in% c("present", "current")) {
    return(as.Date(NA))
  }

  # True Excel serial dates are normally five digits for modern CV dates.
  if (grepl("^\\d{5}(\\.0+)?$", x_chr)) {
    serial <- suppressWarnings(as.numeric(x_chr))
    if (!is.na(serial)) {
      return(as.Date(serial, origin = "1899-12-30"))
    }
  }

  # YYYY-MM
  if (grepl("^\\d{4}-\\d{1,2}$", x_chr)) {
    d <- suppressWarnings(as.Date(paste0(x_chr, "-01"), format = "%Y-%m-%d"))
    if (!is.na(d)) return(d)
  }

  # Month-year values such as Apr-2026, April 2026, Apr/2026.
  if (grepl("^[A-Za-z]{3,9}[- /]\\d{4}$", x_chr)) {
    cleaned <- gsub("[ /]", "-", x_chr)

    for (fmt in c("%d-%b-%Y", "%d-%B-%Y")) {
      d <- suppressWarnings(as.Date(paste0("01-", cleaned), format = fmt))
      if (!is.na(d)) return(d)
    }
  }

  # Full dates are accepted, but display precision is reduced to month-year.
  full_date_formats <- c(
    "%d-%b-%Y",
    "%d-%B-%Y",
    "%d %b %Y",
    "%d %B %Y",
    "%Y-%m-%d",
    "%d/%m/%Y",
    "%d-%m-%Y"
  )

  for (fmt in full_date_formats) {
    d <- suppressWarnings(as.Date(x_chr, format = fmt))
    if (!is.na(d)) return(d)
  }

  as.Date(NA)
}


format_cv_date_one <- function(x) {

  if (is_blank_cv_date(x)) {
    return("")
  }

  x_chr <- trimws(as.character(x))

  # Preserve year-only entries exactly as year-only.
  if (grepl("^\\d{4}$", x_chr)) {
    return(x_chr)
  }

  # Preserve words used for an open-ended date range.
  if (tolower(x_chr) %in% c("present", "current")) {
    return(tolower(x_chr))
  }

  d <- parse_cv_date_one(x)

  if (!is.na(d)) {
    return(format(d, "%b-%Y"))
  }

  # If a value is intentionally non-standard text, leave it untouched.
  x_chr
}


format_cv_date <- function(x) {
  vapply(
    seq_along(x),
    function(i) format_cv_date_one(x[i]),
    character(1)
  )
}


format_cv_date_range <- function(start_val, end_val) {

  n <- max(length(start_val), length(end_val))

  if (n == 0) {
    return(character(0))
  }

  start_val <- rep(start_val, length.out = n)
  end_val   <- rep(end_val, length.out = n)

  vapply(
    seq_len(n),
    function(i) {

      start_fmt <- format_cv_date_one(start_val[i])
      end_fmt   <- format_cv_date_one(end_val[i])

      if (end_fmt == "" || identical(start_fmt, end_fmt)) {
        start_fmt
      } else {
        paste0(start_fmt, " – ", end_fmt)
      }
    },
    character(1)
  )
}


cv_date_sort <- function(x) {
  vapply(
    seq_along(x),
    function(i) {
      d <- parse_cv_date_one(x[i])
      if (is.na(d)) NA_real_ else as.numeric(d)
    },
    numeric(1)
  )
}

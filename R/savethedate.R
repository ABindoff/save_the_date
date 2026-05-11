
#' Parse almost anything into a fuzzy datetime
#' @param x A character vector
#' @return A fuzzy_dt object
#' @export
parse_dt <- function(x) {
  x <- as.character(x)
  n <- length(x)
  
  # Initial parse with parsedate
  parsed <- parsedate::parse_date(x)
  
  # Extraction with regex for verification
  # Year: 4 digits starting with 19 or 20
  has_year <- grepl("\\b(19|20)\\d{2}\\b", x)
  
  # Month: names
  month_names <- "january|february|march|april|may|june|july|august|september|october|november|december|jan|feb|mar|apr|may|jun|jul|aug|sep|oct|nov|dec"
  has_month_name <- grepl(paste0("(?i)\\b(", month_names, ")\\b"), x, perl = TRUE)
  
  # Time: AM/PM or HH:MM
  has_time <- grepl("(?i)(\\d{1,2}(:\\d{2})?\\s*([ap]m))|(\\d{1,2}:\\d{2}(:\\d{2})?)", x, perl = TRUE)
  
  # Day: numbers 1-31 (tricky if multiple numbers)
  # But usually if parsedate works and we have a month, we have a day.
  # If string is just "2026", parsedate gives 2026-01-01.
  # We check if day/month was actually in the string.
  
  # Simple heuristic: if string has only 4 digits and it's a year, then no month/day
  just_year <- grepl("^\\s*\\d{4}\\s*$", x)
  
  res <- list(
    year = as.integer(lubridate::year(parsed)),
    month = as.integer(lubridate::month(parsed)),
    day = as.integer(lubridate::day(parsed)),
    hour = as.integer(lubridate::hour(parsed)),
    minute = as.integer(lubridate::minute(parsed)),
    second = as.integer(lubridate::second(parsed))
  )
  
  # Apply corrections
  res$year[!has_year] <- NA
  res$hour[!has_time] <- NA
  res$minute[!has_time] <- NA
  res$second[!has_time] <- NA
  
  # If it's just a year, set month/day to NA
  res$month[just_year] <- NA
  res$day[just_year] <- NA
  
  # If it's just a time (e.g., "9pm"), parsedate might have filled today's date
  # Check if month name or numeric date pattern exists
  has_date_part <- grepl("(?i)([a-z]{3,})|(\\d{1,2}[/-]\\d{1,2})|(\\d{4}[/-])", x, perl = TRUE) | has_year | has_month_name
  res$month[!has_date_part] <- NA
  res$day[!has_date_part] <- NA
  
  # Handle failures
  res$year[is.na(parsed)] <- NA
  res$month[is.na(parsed)] <- NA
  res$day[is.na(parsed)] <- NA
  
  class(res) <- c("fuzzy_dt", "list")
  res
}

#' @export
print.fuzzy_dt <- function(x, ...) {
  n <- length(x$year)
  y <- ifelse(is.na(x$year), "NA", sprintf("%04d", x$year))
  m <- ifelse(is.na(x$month), "NA", sprintf("%02d", x$month))
  d <- ifelse(is.na(x$day), "NA", sprintf("%02d", x$day))
  h <- ifelse(is.na(x$hour), "NA", sprintf("%02d", x$hour))
  mi <- ifelse(is.na(x$minute), "NA", sprintf("%02d", x$minute))
  s <- ifelse(is.na(x$second), "NA", sprintf("%02d", x$second))
  
  out <- paste0(y, "-", m, "-", d, " ", h, ":", mi, ":", s)
  print(out, ...)
}

#' @export
as.POSIXct.fuzzy_dt <- function(x, tz = "UTC", ...) {
  curr <- as.POSIXlt(Sys.time())
  yy <- ifelse(is.na(x$year), curr$year + 1900, x$year)
  mm <- ifelse(is.na(x$month), 1, x$month)
  dd <- ifelse(is.na(x$day), 1, x$day)
  hh <- ifelse(is.na(x$hour), 0, x$hour)
  mi <- ifelse(is.na(x$minute), 0, x$minute)
  ss <- ifelse(is.na(x$second), 0, x$second)
  ISOdatetime(yy, mm, dd, hh, mi, ss, tz = tz)
}

#' @export
as.data.frame.fuzzy_dt <- function(x, ...) {
  as.data.frame(unclass(x), ...)
}

#' @export
as.character.fuzzy_dt <- function(x, ...) {
  y <- ifelse(is.na(x$year), "NA", sprintf("%04d", x$year))
  m <- ifelse(is.na(x$month), "NA", sprintf("%02d", x$month))
  d <- ifelse(is.na(x$day), "NA", sprintf("%02d", x$day))
  h <- ifelse(is.na(x$hour), "NA", sprintf("%02d", x$hour))
  mi <- ifelse(is.na(x$minute), "NA", sprintf("%02d", x$minute))
  s <- ifelse(is.na(x$second), "NA", sprintf("%02d", x$second))
  paste0(y, "-", m, "-", d, " ", h, ":", mi, ":", s)
}

#' @export
`[.fuzzy_dt` <- function(x, i) {
  res <- lapply(unclass(x), `[`, i)
  class(res) <- c("fuzzy_dt", "list")
  res
}

#' @export
length.fuzzy_dt <- function(x) {
  length(x$year)
}

#' @export
Ops.fuzzy_dt <- function(e1, e2) {
  if (.Generic == "-") {
    return(difftime(as.POSIXct(e1), as.POSIXct(e2)))
  }
  stop("Operation not supported for fuzzy_dt")
}

#' @export
year <- function(x) UseMethod("year")
#' @export
year.fuzzy_dt <- function(x) x$year

#' @export
month <- function(x) UseMethod("month")
#' @export
month.fuzzy_dt <- function(x) x$month

#' @export
day <- function(x) UseMethod("day")
#' @export
day.fuzzy_dt <- function(x) x$day

#' @export
hour <- function(x) UseMethod("hour")
#' @export
hour.fuzzy_dt <- function(x) x$hour

#' @export
minute <- function(x) UseMethod("minute")
#' @export
minute.fuzzy_dt <- function(x) x$minute

#' @export
second <- function(x) UseMethod("second")
#' @export
second.fuzzy_dt <- function(x) x$second

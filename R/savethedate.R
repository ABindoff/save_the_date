
#' Parse almost anything into a fuzzy datetime
#' @param x A character vector
#' @param ref_date A reference date for relative dates (e.g. "yesterday"). Defaults to current date.
#' @return A fuzzy_dt object
#' @export
parse_dt <- function(x, ref_date = Sys.Date()) {
  x <- as.character(x)
  n <- length(x)
  ref_date <- as.Date(ref_date)
  
  # Initial parse with parsedate
  parsed <- parsedate::parse_date(x)
  
  # Handle relative dates by shifting based on ref_date
  today <- as.Date(Sys.time())
  if (ref_date != today) {
    diff <- as.numeric(ref_date - today)
    is_relative <- grepl("(?i)yesterday|today|tomorrow|last|next|ago|hence|now", x)
    parsed[is_relative] <- parsed[is_relative] + (diff * 86400)
  }
  
  # Month detection logic
  month_names_vec <- c("january", "february", "march", "april", "may", "june", 
                       "july", "august", "september", "october", "november", "december")
  month_shorts_vec <- c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
  
  # Helper to find month in string
  find_month <- function(s) {
    if (is.na(s)) return(NA_integer_)
    
    # 1. Check exact names
    for (i in 1:12) {
      if (grepl(paste0("(?i)\\b", month_names_vec[i], "\\b"), s) || 
          grepl(paste0("(?i)\\b", month_shorts_vec[i], "\\b"), s)) {
        return(i)
      }
    }
    
    # 2. Check fuzzy names (max ~20% edits)
    for (i in 1:12) {
      if (length(agrep(month_names_vec[i], s, max.distance = 0.2, ignore.case = TRUE)) > 0) {
        return(i)
      }
    }
    
    # 3. Check numeric date patterns (e.g. 10/02/2025)
    # We trust parsedate if it looks like it found a numeric date
    if (grepl("\\b\\d{1,2}[/-]\\d{1,2}[/-](\\d{2}|\\d{4})\\b", s) || 
        grepl("\\b\\d{4}[/-]\\d{1,2}[/-]\\d{1,2}\\b", s)) {
      return(as.integer(lubridate::month(parsedate::parse_date(s))))
    }
    
    return(NA_integer_)
  }
  
  detected_months <- sapply(x, find_month)
  
  # Year: 4 digits starting with 19 or 20
  has_year <- grepl("\\b(19|20)\\d{2}\\b", x)
  
  # Time: AM/PM or HH:MM
  has_time <- grepl("(?i)(\\d{1,2}(:\\d{2})?\\s*([ap]m))|(\\d{1,2}:\\d{2}(:\\d{2})?)", x, perl = TRUE)
  
  # Simple heuristic: if string has only 4 digits and it's a year, then no month/day
  just_year <- grepl("^\\s*\\d{4}\\s*$", x)
  
  res <- list(
    year = as.integer(lubridate::year(parsed)),
    month = as.integer(detected_months),
    day = as.integer(lubridate::day(parsed)),
    hour = as.integer(lubridate::hour(parsed)),
    minute = as.integer(lubridate::minute(parsed)),
    second = as.integer(lubridate::second(parsed))
  )
  
  # Adjust for relative dates (overwrite the detected months which would be NA)
  is_relative <- grepl("(?i)yesterday|today|tomorrow|last|next|ago|hence|now", x)
  res$month[is_relative] <- as.integer(lubridate::month(parsed[is_relative]))
  res$day[is_relative] <- as.integer(lubridate::day(parsed[is_relative]))
  
  # Apply corrections
  res$year[!has_year] <- NA
  res$hour[!has_time] <- NA
  res$minute[!has_time] <- NA
  res$second[!has_time] <- NA
  
  # If month is NA and not relative, day should also be NA
  res$day[is.na(res$month) & !is_relative] <- NA
  
  # If it's just a year, set month/day to NA
  res$month[just_year] <- NA
  res$day[just_year] <- NA
  
  # Handle failures
  res$year[is.na(parsed)] <- NA
  res$month[is.na(parsed)] <- NA
  res$day[is.na(parsed)] <- NA
  
  class(res) <- c("fuzzy_dt", "list")
  res
}

#' @export
print.fuzzy_dt <- function(x, ...) {
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
as.POSIXct.fuzzy_dt <- function(x, tz = "UTC", ref_date = Sys.Date(), ...) {
  curr <- as.POSIXlt(ref_date)
  yy <- ifelse(is.na(x$year), curr$year + 1900, x$year)
  mm <- ifelse(is.na(x$month), curr$mon + 1, x$month)
  dd <- ifelse(is.na(x$day), curr$mday, x$day)
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

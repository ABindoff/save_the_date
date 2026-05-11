
#' Parse almost anything into a fuzzy datetime
#' @param x A character vector
#' @param ref_date A reference date for relative dates (e.g. "yesterday"). Defaults to current date.
#' @return A fuzzy_dt object
#' @export
parse_dt <- function(x, ref_date = Sys.Date()) {
  x <- as.character(x)
  n <- length(x)
  
  # Resolve ref_date
  if (is.character(ref_date)) {
    ref_resolved <- parse_dt(ref_date, ref_date = Sys.Date())
    ref_date <- as.Date(as.POSIXct(ref_resolved))
  }
  ref_date <- as.Date(ref_date)
  
  # Core Parsing logic
  # 1. Month detection (Fuzzy + Exact)
  month_names_vec <- c("january", "february", "march", "april", "may", "june", 
                       "july", "august", "september", "october", "november", "december")
  month_shorts_vec <- c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
  
  find_month <- function(s) {
    if (is.na(s)) return(NA_integer_)
    for (i in 1:12) {
      if (grepl(paste0("(?i)\\b", month_names_vec[i], "\\b"), s) || 
          grepl(paste0("(?i)\\b", month_shorts_vec[i], "\\b"), s)) return(i)
    }
    for (i in 1:12) {
      if (length(agrep(month_names_vec[i], s, max.distance = 0.2, ignore.case = TRUE)) > 0) return(i)
    }
    # Check numeric formats
    if (grepl("\\b\\d{1,2}[/-]\\d{1,2}[/-](\\d{2}|\\d{4})\\b", s) || 
        grepl("\\b\\d{4}[/-]\\d{1,2}[/-]\\d{1,2}\\b", s)) {
      tmp <- parsedate::parse_date(s)
      if (!is.na(tmp)) return(as.integer(lubridate::month(tmp)))
    }
    return(NA_integer_)
  }
  
  # 2. Natural Language Parsing
  parse_natural <- function(s, ref) {
    s <- tolower(trimws(s))
    if (s == "today") return(ref)
    if (s == "tomorrow") return(ref + 1)
    if (s == "yesterday") return(ref - 1)
    
    # Next/Last Weekday
    weekdays <- c("sunday", "monday", "tuesday", "wednesday", "thursday", "friday", "saturday")
    if (grepl("^(next|last)\\s+(sunday|monday|tuesday|wednesday|thursday|friday|saturday)$", s)) {
      parts <- strsplit(s, "\\s+")[[1]]
      dir <- if (parts[1] == "next") 1 else -1
      wday_target <- which(weekdays == parts[2]) - 1 # 0-indexed
      wday_ref <- as.POSIXlt(ref)$wday
      
      diff <- wday_target - wday_ref
      if (dir == 1 && diff <= 0) diff <- diff + 7
      if (dir == -1 && diff >= 0) diff <- diff - 7
      return(ref + diff)
    }
    
    # [n] units ago/hence
    if (grepl("^(\\d+)\\s+(day|week|month|year)s?\\s+(ago|hence)$", s)) {
      parts <- strsplit(s, "\\s+")[[1]]
      n_val <- as.numeric(parts[1])
      unit <- parts[2]
      dir <- if (parts[3] == "ago") -1 else 1
      
      if (grepl("day", unit)) return(ref + (n_val * dir))
      if (grepl("week", unit)) return(ref + (n_val * 7 * dir))
      # Months/Years are approximate in days here for simplicity or we use POSIXlt
      lt <- as.POSIXlt(ref)
      if (grepl("month", unit)) {
        lt$mon <- lt$mon + (n_val * dir)
        return(as.Date(lt))
      }
      if (grepl("year", unit)) {
        lt$year <- lt$year + (n_val * dir)
        return(as.Date(lt))
      }
    }
    
    if (s == "next week") return(ref + 7)
    if (s == "last week") return(ref - 7)
    
    return(as.Date(NA))
  }
  
  # Process inputs
  results_list <- lapply(x, function(s) {
    nat <- parse_natural(s, ref_date)
    if (!is.na(nat)) {
      return(list(
        year = as.integer(lubridate::year(nat)),
        month = as.integer(lubridate::month(nat)),
        day = as.integer(lubridate::day(nat)),
        hour = NA_integer_, minute = NA_integer_, second = NA_integer_,
        is_rel = TRUE
      ))
    }
    
    p <- parsedate::parse_date(s)
    m <- find_month(s)
    y <- if (grepl("\\b(19|20)\\d{2}\\b", s)) as.integer(lubridate::year(p)) else NA_integer_
    h <- if (grepl("(?i)(\\d{1,2}(:\\d{2})?\\s*([ap]m))|(\\d{1,2}:\\d{2})", s)) as.integer(lubridate::hour(p)) else NA_integer_
    mi <- if (!is.na(h)) as.integer(lubridate::minute(p)) else NA_integer_
    se <- if (!is.na(h)) as.integer(lubridate::second(p)) else NA_integer_
    
    list(
      year = y,
      month = m,
      day = if (!is.na(m)) as.integer(lubridate::day(p)) else NA_integer_,
      hour = h, minute = mi, second = se,
      is_rel = FALSE
    )
  })
  
  # Combine into fuzzy_dt
  res <- list(
    year = sapply(results_list, `[[`, "year"),
    month = sapply(results_list, `[[`, "month"),
    day = sapply(results_list, `[[`, "day"),
    hour = sapply(results_list, `[[`, "hour"),
    minute = sapply(results_list, `[[`, "minute"),
    second = sapply(results_list, `[[`, "second")
  )
  
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
  if (is.character(ref_date)) {
    ref_resolved <- parse_dt(ref_date, ref_date = Sys.Date())
    ref_date <- as.Date(as.POSIXct(ref_resolved))
  }
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

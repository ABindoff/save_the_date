
#' Parse almost anything into a fuzzy datetime
#' @param x A character vector
#' @param ref_date A reference date for relative dates (e.g. "yesterday"). Defaults to current date.
#' @param verbose Logical. If TRUE, provides a summary of parsing decisions. Defaults to TRUE.
#' @return A fuzzy_dt object
#' @export
parse_dt <- function(x, ref_date = Sys.Date(), verbose = TRUE) {
  x <- as.character(x)
  n <- length(x)
  
  # Registry for summary messaging
  stats <- list(
    ranges = 0,
    relative = 0,
    contextual = 0,
    fuzzy_month = 0,
    vagueness_discarded = 0
  )
  
  # Resolve ref_date
  if (is.character(ref_date)) {
    ref_s <- tolower(trimws(ref_date))
    if (ref_s == "today") ref_date <- Sys.Date()
    else if (ref_s == "yesterday") ref_date <- Sys.Date() - 1
    else if (ref_s == "tomorrow") ref_date <- Sys.Date() + 1
    else {
      if (grepl("^\\d{4}[-./]\\d{1,2}[-./]\\d{1,2}$", ref_s)) {
        ref_date <- as.Date(gsub("[./]", "-", ref_s))
      } else {
        tmp_p <- parsedate::parse_date(ref_date)
        ref_date <- if (is.na(tmp_p)) Sys.Date() else as.Date(tmp_p)
      }
    }
  }
  ref_date <- as.Date(ref_date)
  
  # --- STEP 0: RANGE DETECTION ---
  for (i in seq_along(x)) {
    if (!is.na(x[i]) && grepl(".+[-–—].+", x[i])) {
      stats$ranges <- stats$ranges + 1
      x[i] <- trimws(strsplit(x[i], "[-–—]")[[1]][1])
    }
  }

  # --- STEP 1: CONTEXT ---
  slots_list <- lapply(x, function(s) {
    if (is.na(s)) return(NULL)
    m <- gregexpr("\\b\\d{1,4}\\b", s)
    res <- as.integer(regmatches(s, m)[[1]])
    if (length(res) == 3) return(res) else return(NULL)
  })
  mappings <- list(dmy = c(3, 2, 1), mdy = c(3, 1, 2), ymd = c(1, 2, 3), ydm = c(1, 3, 2))
  scores <- rep(0, length(mappings)); names(scores) <- names(mappings)
  possible <- rep(TRUE, length(mappings)); names(possible) <- names(mappings)
  curr_y <- as.integer(lubridate::year(ref_date))

  for (slots in slots_list) {
    if (is.null(slots)) next
    for (m_name in names(mappings)) {
      if (!possible[m_name]) next
      m <- mappings[[m_name]]; y <- slots[m[1]]; mo <- slots[m[2]]; d <- slots[m[3]]
      if (is.na(mo) || mo < 1 || mo > 12 || is.na(d) || d < 1 || d > 31) { possible[m_name] <- FALSE; next }
      if (y > 1000) scores[m_name] <- scores[m_name] + 100 
      else if (y > 31) scores[m_name] <- scores[m_name] + 10
      if (d > 12) scores[m_name] <- scores[m_name] + 5
      y_full <- if (y < 100) ifelse(y > 50, 1900 + y, 2000 + y) else y
      if (abs(y_full - curr_y) <= 1) scores[m_name] <- scores[m_name] + 20
    }
  }
  consensus <- if (any(possible)) names(scores)[possible][which.max(scores[possible])] else NULL
  
  # --- STEP 2: PARSING ---
  month_names <- c("january", "february", "march", "april", "may", "june", "july", "august", "september", "october", "november", "december")
  month_shorts <- c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
  yy <- rep(NA_integer_, n); mm <- rep(NA_integer_, n); dd <- rep(NA_integer_, n)
  hh <- rep(NA_integer_, n); mi <- rep(NA_integer_, n); ss <- rep(NA_integer_, n)

  for (i in 1:n) {
    s <- trimws(x[i])
    if (is.na(s) || s == "") next
    s_low <- tolower(s)
    
    if (grepl("(?i)\\b\\d{2}'?s\\b", s)) {
      stats$vagueness_discarded <- stats$vagueness_discarded + 1
      next
    }

    # Military
    if (grepl("^\\s*\\d{4}\\s*$", s) && !grepl("^\\s*(19|20)\\d{2}\\s*$", s)) {
       hv <- as.integer(substr(s, 1, 2)); mv <- as.integer(substr(s, 3, 4))
       if (hv < 24 && mv < 60) { hh[i] <- hv; mi[i] <- mv; ss[i] <- 0L; next }
    }

    # Relative
    if (s_low %in% c("today", "tomorrow", "yesterday", "noon", "midnight")) {
      stats$relative <- stats$relative + 1
      if (s_low == "today") { yy[i] <- as.integer(lubridate::year(ref_date)); mm[i] <- as.integer(lubridate::month(ref_date)); dd[i] <- as.integer(lubridate::day(ref_date)); next }
      if (s_low == "tomorrow") { dr <- ref_date + 1; yy[i] <- as.integer(lubridate::year(dr)); mm[i] <- as.integer(lubridate::month(dr)); dd[i] <- as.integer(lubridate::day(dr)); next }
      if (s_low == "yesterday") { dr <- ref_date - 1; yy[i] <- as.integer(lubridate::year(dr)); mm[i] <- as.integer(lubridate::month(dr)); dd[i] <- as.integer(lubridate::day(dr)); next }
      if (s_low == "noon") { yy[i] <- as.integer(lubridate::year(ref_date)); mm[i] <- as.integer(lubridate::month(ref_date)); dd[i] <- as.integer(lubridate::day(ref_date)); hh[i] <- 12; mi[i] <- 0; next }
      if (s_low == "midnight") { yy[i] <- as.integer(lubridate::year(ref_date)); mm[i] <- as.integer(lubridate::month(ref_date)); dd[i] <- as.integer(lubridate::day(ref_date)); hh[i] <- 0; mi[i] <- 0; next }
    }

    # o'clock
    if (grepl("(\\d{1,2})\\s*o'clock", s_low)) {
       m <- regexec("(\\d{1,2})\\s*o'clock", s_low)
       hr <- as.integer(regmatches(s_low, m)[[1]][2])
       if (grepl("afternoon|evening|p\\.m\\.|pm", s_low) && hr < 12) hr <- hr + 12
       if (grepl("morning|a\\.m\\.|am", s_low) && hr == 12) hr <- 0
       hh[i] <- hr; mi[i] <- 0L; ss[i] <- 0L; stats$relative <- stats$relative + 1
    }

    # next/last weekday
    if (grepl("(next|last)\\s+(monday|tuesday|wednesday|thursday|friday|saturday|sunday)", s_low)) {
       stats$relative <- stats$relative + 1
       m <- regexec("(next|last)\\s+(monday|tuesday|wednesday|thursday|friday|saturday|sunday)", s_low)
       parts <- regmatches(s_low, m)[[1]]
       dir <- if (parts[2] == "next") 1 else -1
       wday_target <- match(parts[3], c("sunday","monday","tuesday","wednesday","thursday","friday","saturday"))
       wday_curr <- as.integer(lubridate::wday(ref_date))
       diff <- wday_target - wday_curr
       if (dir == 1 && diff <= 0) diff <- diff + 7
       if (dir == -1 && diff >= 0) diff <- diff - 7
       dr <- ref_date + diff
       yy[i] <- as.integer(lubridate::year(dr)); mm[i] <- as.integer(lubridate::month(dr)); dd[i] <- as.integer(lubridate::day(dr))
    }

    # Contextual resolution
    if (is.na(yy[i]) && !is.null(consensus) && !is.null(slots_list[[i]])) {
       stats$contextual <- stats$contextual + 1
       m <- mappings[[consensus]]; slots <- slots_list[[i]]; y_v <- slots[m[1]]
       if (y_v < 100) y_v <- ifelse(y_v > 50, 1900 + y_v, 2000 + y_v)
       yy[i] <- y_v; mm[i] <- slots[m[2]]; dd[i] <- slots[m[3]]
       pt <- parsedate::parse_date(s); if (!is.na(pt) && grepl("(?i)(\\d{1,2}(:\\d{2})?\\s*([ap]m))|(\\d{1,2}:\\d{2})", s)) {
         hh[i] <- as.integer(lubridate::hour(pt)); mi[i] <- as.integer(lubridate::minute(pt)); ss[i] <- as.integer(lubridate::second(pt))
       }
       next
    }

    # Fallback to standard
    if (is.na(yy[i])) {
      p <- parsedate::parse_date(gsub("\\.", "/", s))
      if (!is.na(p)) {
        m_idx <- NA_integer_
        for (j in 1:12) {
          if (grepl(paste0("(?i)\\b", month_names[j], "\\b"), s) || grepl(paste0("(?i)\\b", month_shorts[j], "\\b"), s)) {
            m_idx <- j; break
          } else if (length(agrep(month_names[j], s, max.distance = 0.25, ignore.case = TRUE)) > 0) {
            m_idx <- j; stats$fuzzy_month <- stats$fuzzy_month + 1; break
          }
        }
        
        y_cand <- as.integer(lubridate::year(p))
        m_y <- gregexpr("\\b(19|20)\\d{2}\\b|\\b\\d{2}$", s)
        has_year_in_str <- any(regmatches(s, m_y)[[1]] != "")
        
        if (has_year_in_str || grepl("(?i)(next|last|ago|tomorrow|yesterday|today)", s)) {
          yy[i] <- y_cand
        }
        
        if (is.na(m_idx)) m_idx <- as.integer(lubridate::month(p))
        mm[i] <- m_idx; dd[i] <- as.integer(lubridate::day(p))
        
        if (is.na(hh[i]) && grepl("(?i)(\\d{1,2}(:\\d{2})?\\s*([ap]m))|(\\d{1,2}:\\d{2})|o'clock", s)) {
          hh[i] <- as.integer(lubridate::hour(p)); mi[i] <- as.integer(lubridate::minute(p)); ss[i] <- as.integer(lubridate::second(p))
        }
      }
    }
    
    # Phrase-relative
    if (is.na(mm[i]) && grepl("(?i)(one|two|three|four|five|six|seven|eight|nine|ten|\\d+)\\s+(day|week|month|year)s?\\s+ago", s)) {
       stats$relative <- stats$relative + 1
       words <- c("one"=1, "two"=2, "three"=3, "four"=4, "five"=5, "six"=6, "seven"=7, "eight"=8, "nine"=9, "ten"=10)
       m <- regexec("(?i)(one|two|three|four|five|six|seven|eight|nine|ten|\\d+)\\s+(day|week|month|year)s?\\s+ago", s)
       parts <- regmatches(s, m)[[1]]
       n_v <- if (parts[2] %in% names(words)) words[parts[2]] else as.numeric(parts[2])
       unit <- tolower(parts[3]); dr <- ref_date
       if (unit == "day") dr <- ref_date - n_v
       else if (unit == "week") dr <- ref_date - (n_v * 7)
       else { lt <- as.POSIXlt(ref_date); if (unit == "month") lt$mon <- lt$mon - n_v else lt$year <- lt$year - n_v; dr <- as.Date(lt) }
       yy[i] <- as.integer(lubridate::year(dr)); mm[i] <- as.integer(lubridate::month(dr)); dd[i] <- as.integer(lubridate::day(dr))
    }
  }
  
  # Final Audited Messaging
  if (verbose) {
    if (!is.null(consensus)) message("Note: Column-wide format resolved as '", consensus, "'.")
    if (stats$relative > 0) message("Note: ", stats$relative, " relative dates parsed (anchored to ", ref_date, ").")
    if (stats$fuzzy_month > 0) message("Note: ", stats$fuzzy_month, " months corrected via fuzzy matching (e.g. Septober).")
    if (stats$ranges > 0) warning(stats$ranges, " date ranges detected; only the first component was parsed.")
    if (stats$vagueness_discarded > 0) message("Note: ", stats$vagueness_discarded, " vague terms (decades) were discarded.")
  }

  res <- data.frame(year = yy, month = mm, day = dd, hour = hh, minute = mi, second = ss)
  class(res) <- c("fuzzy_dt", "data.frame")
  res
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
Ops.fuzzy_dt <- function(e1, e2) {
  if (.Generic == "-") return(difftime(as.POSIXct(e1), as.POSIXct(e2)))
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

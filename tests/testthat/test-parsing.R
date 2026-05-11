
test_that("Standard dates parse correctly", {
  # Using ISO format to avoid MM/DD vs DD/MM ambiguity in base tests
  x <- c("2025-02-10", "2025/02/10")
  dt <- parse_dt(x)
  expect_equal(year(dt), rep(2025L, 2))
  expect_equal(month(dt), rep(2L, 2))
  expect_equal(day(dt), rep(10L, 2))
})

test_that("Fuzzy month matching works", {
  x <- c("18th Februry 2025", "18th Septober 2025", "18th Octember 2025")
  dt <- parse_dt(x)
  expect_equal(month(dt), c(2L, 9L, 10L))
  expect_equal(day(dt), rep(18L, 3))
})

test_that("Relative dates work with default anchor", {
  dt <- parse_dt("today")
  expect_equal(as.integer(year(dt)), as.integer(lubridate::year(Sys.Date())))
  expect_equal(as.integer(month(dt)), as.integer(lubridate::month(Sys.Date())))
  expect_equal(as.integer(day(dt)), as.integer(lubridate::day(Sys.Date())))
})

test_that("Relative dates work with custom ref_date", {
  ref <- as.Date("2020-01-01") # Wednesday
  
  # Today/Tomorrow/Yesterday
  expect_equal(day(parse_dt("today", ref_date = ref)), 1L)
  expect_equal(month(parse_dt("today", ref_date = ref)), 1L)
  expect_equal(year(parse_dt("today", ref_date = ref)), 2020L)
  
  expect_equal(day(parse_dt("yesterday", ref_date = ref)), 31L)
  expect_equal(month(parse_dt("yesterday", ref_date = ref)), 12L)
  expect_equal(year(parse_dt("yesterday", ref_date = ref)), 2019L)
  
  # Next/Last Weekday
  # Last Friday before Wednesday Jan 1st 2020 is Dec 27th 2019
  dt_last <- parse_dt("last friday", ref_date = ref)
  expect_equal(month(dt_last), 12L)
  expect_equal(day(dt_last), 27L)
  
  # Next Friday is Jan 3rd 2020
  dt_next <- parse_dt("next friday", ref_date = ref)
  expect_equal(month(dt_next), 1L)
  expect_equal(day(dt_next), 3L)
})

test_that("Ref date as a string works", {
  dt <- parse_dt("tomorrow", ref_date = "2026-05-11")
  expect_equal(day(dt), 12L)
  expect_equal(month(dt), 5L)
  
  dt2 <- parse_dt("last friday", ref_date = "yesterday")
  # Should successfully parse without error
  expect_false(is.na(month(dt2)))
})

test_that("Garbage returns NA month/day safely", {
  x <- c("10th of floobuary 2025", "some random text")
  dt <- parse_dt(x)
  expect_true(all(is.na(month(dt))))
  expect_true(all(is.na(day(dt))))
})

test_that("Arithmetic and difftime work", {
  dt1 <- parse_dt("2025-01-01")
  dt2 <- parse_dt("2025-01-02")
  diff <- dt2 - dt1
  expect_equal(as.numeric(diff, units = "days"), 1)
  
  # Regression tests for timezone-related shifts
  t1 <- parse_dt("today")
  t2 <- parse_dt("today", ref_date = "yesterday")
  expect_equal(as.numeric(difftime(t1, t2), units = "days"), 1)
  
  t3 <- parse_dt("yesterday")
  expect_equal(as.numeric(difftime(t1, t3), units = "days"), 1)
})

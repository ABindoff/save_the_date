
test_that("Contextual parsing resolves ambiguous dates", {
  expect_equal(month(parse_dt(c("02/01/26", "13/10/26")))[1], 1L)
  expect_equal(day(parse_dt(c("02/01/26", "13/10/26")))[1], 2L)
  expect_equal(year(parse_dt(c("02/01/26", "13/10/26")))[1], 2026L)
})

test_that("Fuzzy month matching works", {
  expect_equal(month(parse_dt("18th Februry 2025")), 2L)
  expect_equal(month(parse_dt("18th Septober 2025")), 9L)
  expect_equal(month(parse_dt("18th Octember 2025")), 10L)
})

test_that("Relative dates work with explicit anchors", {
  expect_equal(day(parse_dt("yesterday", ref_date = "2026-05-12")), 11L)
  expect_equal(day(parse_dt("tomorrow", ref_date = "2026-05-12")), 13L)
  expect_equal(hour(parse_dt("noon", ref_date = "2026-05-12")), 12L)
  
  # May 28 - 14 days = May 14
  res_ago <- parse_dt("two weeks ago", ref_date = "2026-05-28")
  expect_equal(month(res_ago), 5L)
  expect_equal(day(res_ago), 14L)
})

test_that("Military and standard formats work", {
  expect_equal(hour(parse_dt("1530")), 15L)
  expect_equal(minute(parse_dt("1530")), 30L)
  expect_equal(month(parse_dt("3.15.2025")), 3L)
  expect_equal(day(parse_dt("3.15.2025")), 15L)
})

test_that("Garbage returns NA safely", {
  expect_true(is.na(month(parse_dt("some random text"))))
  expect_true(is.na(day(parse_dt("some random text"))))
})

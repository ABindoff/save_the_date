
test_that("Contextual parsing resolves ambiguous dates", {
  # 02/01/26 is ambiguous (Feb 1 vs Jan 2)
  # 13/10/26 implies DMY (13th Oct)
  x <- c("02/01/26", "13/10/26")
  res <- parse_dt(x)
  expect_equal(month(res)[1], 1L)
  expect_equal(day(res)[1], 2L)
  expect_equal(year(res)[1], 2026L)
})

test_that("Fuzzy month matching works", {
  expect_equal(month(parse_dt("18th Februry 2025")), 2L)
  expect_equal(month(parse_dt("18th Septober 2025")), 9L)
  expect_equal(month(parse_dt("18th Octember 2025")), 10L)
})

test_that("Relative dates with static anchors work", {
  ref <- "2026-05-12" # A Tuesday
  
  expect_equal(day(parse_dt("yesterday", ref_date = ref)), 11L)
  expect_equal(day(parse_dt("tomorrow", ref_date = ref)), 13L)
  expect_equal(hour(parse_dt("noon", ref_date = ref)), 12L)
  
  # phrase relative
  res_ago <- parse_dt("two weeks ago", ref_date = ref)
  expect_equal(day(res_ago), 28L)
  expect_equal(month(res_ago), 4L)
})

test_that("Military and standard formats work", {
  expect_equal(hour(parse_dt("1530")), 15L)
  expect_equal(minute(parse_dt("1530")), 30L)
  
  res_dot <- parse_dt("3.15.2025")
  expect_equal(month(res_dot), 3L)
  expect_equal(day(res_dot), 15L)
})

test_that("Garbage returns NA month/day safely", {
  res <- parse_dt("some random text")
  expect_true(is.na(month(res)))
  expect_true(is.na(day(res)))
})

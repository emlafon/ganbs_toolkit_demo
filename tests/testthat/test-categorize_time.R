# Tests for categorize_time() - a good first thing to test since it's
# pure logic (no data files, no internet access needed), so it runs
# fast and reliably in any environment, including R CMD check's.

test_that("categorize_time bins days correctly", {
  # expect_equal() checks that two things are exactly equal - here,
  # that specific input values land in the bins we expect
  expect_equal(
    as.character(categorize_time(0, type = "days")),
    "0 days"
  )
  expect_equal(
    as.character(categorize_time(10, type = "days")),
    "7-14 days"
  )
  expect_equal(
    as.character(categorize_time(100, type = "days")),
    ">14 days"
  )
})

test_that("categorize_time treats negative or missing values as Unknown", {
  expect_equal(as.character(categorize_time(-1, type = "days")), "Unknown")
  expect_equal(as.character(categorize_time(NA, type = "days")), "Unknown")
})

test_that("categorize_time bins hours correctly", {
  expect_equal(as.character(categorize_time(5, type = "hours")), "0-11.9 hours old")
  expect_equal(as.character(categorize_time(50, type = "hours")), "48.1-72 hours old")
})

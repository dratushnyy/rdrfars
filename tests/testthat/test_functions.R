test_that("can_read_existing_file", {
  loaded <- fars_read("accident_2013.csv")
  expect_is(loaded, "data.frame")
  expect_equal(length(loaded), 50)
})

test_that("stop_on_non_existing_file", {
  expect_error(fars_read("not_existing_file"))
})

test_that("warning_on_invalid_year_for_file_name", {
  expect_warning(make_filename("abc"), 'NAs introduced by coercion')
})

test_that("reads_year_data", {
  loaded <- fars_read_years(c(2013))
})

test_that("skip_read_data_on_invalid_year", {
  expect_warning(fars_read_years(c("abc")), 'NAs introduced by coercion')
  expect_warning(fars_read_years(c(2000)), 'invalid year: 2000')
})


test_that("stop_summary_on_invalid_state_id", {
  expect_error(fars_map_state(-1, 2013))
})

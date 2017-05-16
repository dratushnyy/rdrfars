test_that("can_read_existing_file", {
  loaded <- fars_read("test_data_2013.csv")
  expect_is(loaded, "data.frame")
  expect_equal(length(loaded), 50)
})

test_that("stop_on_non_existing_file", {
  expect_error(fars_read("not_existing_file"))
})

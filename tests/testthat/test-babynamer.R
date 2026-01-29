test_that("us_baby_names returns data.frame for each type", {
  skip_if_offline()
  skip_on_cran()

  expect_s3_class(us_baby_names(type = "national"), "data.frame")
  expect_s3_class(us_baby_names(type = "state"), "data.frame")
  expect_s3_class(us_baby_names(type = "territory"), "data.frame")
})

test_that("us_baby_names handles multiple types", {
  skip_if_offline()
  skip_on_cran()

  result <- us_baby_names(type = c("national", "state"))
  expect_s3_class(result, "data.frame")
})

test_that("us_baby_names errors on empty type", {
  expect_error(us_baby_names(type = character(0)))
})

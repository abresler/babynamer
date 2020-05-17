context("Run tests")

test_that("Data can be acquired", {

  expect_that(us_baby_names(type = "national"), is_a("data.frame"))

  expect_that(us_baby_names(type = "state"), is_a("data.frame"))
})



testthat::test_that("Dataset: kva", {
  data("kva", package = "streg")

  testthat::expect_s3_class(object = kva, class = "data.frame")
  testthat::expect_equal(object = nrow(kva), expected = 12)
  testthat::expect_equal(object = ncol(kva), expected = 4)
})

testthat::test_that("Dataset: cancer", {
  data("cancer", package = "streg")

  testthat::expect_s3_class(object = cancer, class = "data.frame")
  testthat::expect_equal(object = nrow(cancer), expected = 48)
  testthat::expect_equal(object = ncol(cancer), expected = 4)
})

testthat::test_that("Testing g() function", {
  testthat::expect_equal(champagneptds2023::g(0), 15)
  testthat::expect_equal(champagneptds2023::g(0.5), 2)
  testthat::expect_equal(champagneptds2023::g(10), 2)
  testthat::expect_equal(champagneptds2023::g(15), 8*log2(6) + 2)
  testthat::expect_equal(champagneptds2023::g(20), 8*log2(6) + 2)
  testthat::expect_equal(champagneptds2023::g(21), 0)
})

testthat::test_that("Testing S3 plot() method", {
  champagne_glass <- f(seq(0, 21, by = 0.01))
  testthat::expect_silent(plot(champagne_glass))
})

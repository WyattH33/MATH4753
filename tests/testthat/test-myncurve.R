test_that("myncurve returns correct mu", {
  res <- MATH4753::myncurve(mu = 10, sigma = 5, a = 6)
  expect_identical(res$mu, 10)
})
test_that("myncurve returns correct sigma", {
  res <- MATH4753::myncurve(mu = 10, sigma = 5, a = 6)
  expect_identical(res$sigma, 5)
})
test_that("myncurve returns correct probability", {
  res <- MATH4753::myncurve(mu = 10, sigma = 5, a = 6)
  expect_equal(res$prob, pnorm(6, 10, 5), tolerance = 1e-12)
})

test_that("random example works", {
  m_feature = 5
  min_leaf = 5
  n_trees = 20
  sample_size = 20

  res <- MeanSplitImprovement(X, Y, sample_size, n_trees, m_feature, min_leaf)

  expect_length(res, ncol(X))
  expect_equal(res, c(2.08, 2.76, 3.72, 1.22, 1.54), tolerance=1e-3)
})


test_that("default values work", {
  res <- MeanSplitImprovement(X, Y)

  expect_length(res, ncol(X))
  expect_equal(res, c(0.71, 3.27, 3.61, 0.73, 0.58), tolerance=1e-3)
})


test_that("f-test parameter works", {
  m_feature = 5
  min_leaf = 5
  n_trees = 20
  sample_size = 20
  alpha_threshold = 0.2

  res <- MeanSplitImprovement(X, Y, sample_size, n_trees, m_feature, min_leaf, alpha_threshold)
  expect_length(res, ncol(X))
  expect_equal(res, c(1.38, 3.00, 3.64, 1.50, 0.82), tolerance=1e-3)
})

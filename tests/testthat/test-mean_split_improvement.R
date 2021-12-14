test_that("random example works", {
  m_feature = 5
  min_leaf = 10
  n_trees = 20
  sample_size = 16

  set.seed(42)
  res <- MeanSplitImprovement(X, Y, sample_size, n_trees, m_feature, min_leaf)
  expect_length(res, ncol(X))
  expect_equal(res, c(0.681, 2.295, 1.559, 0.124, 0.091), tolerance=1e-3)
})


test_that("default values work", {
  set.seed(42)
  res <- MeanSplitImprovement(X, Y)
  expect_length(res, ncol(X))
  expect_equal(res, c(0.523, 3.195, 3.187, 0.816, 0.545), tolerance=1e-3)
})


test_that("f-test parameter works", {
  m_feature = 5
  min_leaf = 10
  n_trees = 20
  sample_size = 16
  alpha_threshold = 0.2

  set.seed(42)
  res <- MeanSplitImprovement(X, Y, sample_size, n_trees, m_feature, min_leaf, alpha_threshold)
  expect_length(res, ncol(X))
  expect_equal(res, c(0.512, 2.277, 1.559, 0.104, 0.000), tolerance=1e-3)
})

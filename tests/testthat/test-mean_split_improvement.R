test_that("random example works", {
  set.seed(49)

  X <- matrix(runif(50*5), 50, 5)
  Y <- matrix(runif(50*2), 50, 2)
  m_feature = 5
  min_leaf = 5
  n_trees = 20
  sample_size = 20

  res <- MeanSplitImprovement(X, Y, sample_size, n_trees, m_feature, min_leaf)

  expect_length(res, ncol(X))
  expect_equal(res, c(1.458, 2.965, 3.947, 1.175, 1.037), tolerance=1e-3)
})


test_that("default values work", {
  set.seed(49)

  X <- matrix(runif(50*5), 50, 5)
  Y <- matrix(runif(50*2), 50, 2)
  res <- MeanSplitImprovement(X, Y)

  expect_length(res, ncol(X))
  expect_equal(res, c(0.807, 2.891, 3.460, 0.623, 0.514), tolerance=1e-3)
})


test_that("f-test parameter works", {
  set.seed(49)

  X <- matrix(runif(50*5), 50, 5)
  Y <- matrix(runif(50*2), 50, 2)
  m_feature = 5
  min_leaf = 5
  n_trees = 20
  sample_size = 20
  f_critical = 2.85

  res <- MeanSplitImprovement(X, Y, sample_size, n_trees, m_feature, min_leaf, critical_F=f_critical)
  expect_length(res, ncol(X))
  expect_equal(res, c(0.830, 2.407, 3.424, 0.576, 0.778), tolerance=1e-3)
})

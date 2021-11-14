test_that("random example works", {
  set.seed(49)

  X <- matrix(runif(50*5), 50, 5)
  Y <- matrix(runif(50*2), 50, 2)
  m_feature = 5
  min_leaf = 5
  n_trees = 20
  sample_size = 20

  res <- MeanOutcomeDifference(X, Y, sample_size, n_trees, m_feature, min_leaf)

  features <- ncol(X)
  outcomes <- ncol(Y)
  expected_dim <- c(features, outcomes)

  expect_equal(dim(res), expected_dim)

  expected_val <- array(
    c(0.085, 0.189, 0.139, 0.137, 0.088, 0.155, 0.103, 0.131, 0.141, 0.137),
    dim = expected_dim
  )
  expect_equal(res, expected_val, tolerance=1e-2)
})


test_that("default values work", {
  set.seed(49)

  X <- matrix(runif(50*5), 50, 5)
  Y <- matrix(runif(50*2), 50, 2)

  res <- MeanOutcomeDifference(X, Y)

  features <- ncol(X)
  outcomes <- ncol(Y)
  expected_dim <- c(features, outcomes)

  expect_equal(dim(res), expected_dim)

  expected_val <- array(
    c(0.191, 0.263, 0.150, 0.251, 0.184, 0.144, 0.275, 0.120, 0.1, 0.199),
    dim = expected_dim
  )
  expect_equal(res, expected_val, tolerance=1e-2)
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

  res <- MeanOutcomeDifference(X, Y, sample_size, n_trees, m_feature, min_leaf, f_critical)

  features <- ncol(X)
  outcomes <- ncol(Y)
  expected_dim <- c(features, outcomes)

  expect_equal(dim(res), expected_dim)

  expected_val <- array(
    c(0.032, 0.000, 0.115, 0.056, 0.057, 0.137, 0.045, 0.031, 0.107, 0.093),
    dim = expected_dim
  )
  expect_equal(res, expected_val, tolerance=1e-2)
})

#' Mean Split Improvement Importance Function
#'
#' @param X Feature matrix
#' @param Y Target matrix
#' @param sample_size Size of random subset for each tree generation
#' @param num_trees Number of Trees to generate
#' @param m_feature Number of randomly selected features considered for a split in each regression tree node, which
#' must be positive integer and less than N (number of input features)
#' @param min_leaf Minimum number of samples in the leaf node. If a node has less than or equal
#' to min_leaf samples, then there will be no splitting in that node and this node
#' will be considered as a leaf node. Valid input is positive integer, which is less
#' than or equal to M (number of training samples)
#'
#' @return Vector of size N x 1
#' @export
#'
#' @examples
#' X = matrix(runif(50*100), 50, 100)
#' Y = matrix(runif(50*5), 50, 5)
#' MeanSplitImprovement(X, Y)

MeanSplitImprovement <- function(X, Y, sample_size=trunc(nrow(X) * 0.8), num_trees=100, m_feature=ncol(X), min_leaf=10) {
  if (ncol(Y) == 1) {
    command = 1
  } else {
    command = 2
  }

  treeMeasures <- sapply(1:num_trees, function(tree_index){
    train_index <- sample(nrow(X), sample_size)
    train_X <- X[train_index,]
    train_Y <- Y[train_index,]

    inv_cov_y = solve(stats::cov(train_Y))
    tree <- MultivariateRandomForest::build_single_tree(train_X, train_Y, m_feature, min_leaf, inv_cov_y, command)

    test_index <- setdiff(1:nrow(X), train_index)
    test_X <- X[test_index, ]
    test_Y <- Y[test_index, ]

    GetImportanceMeasuresForSingleTree(tree, test_X, test_Y, inv_cov_y, command)
  })
  result <- apply(treeMeasures, 1, mean)
  result[is.nan(result)] <- 0
  # aggregate tree results with mean
  return (result)
}


SplitImprovementMeasure <- function(y_test_left, y_test_right, inv_cov_y, command) {
  y_test <- rbind(y_test_left, y_test_right)
  cost <- MultivariateRandomForest::Node_cost(y_test, inv_cov_y, command)

  cost_left <- MultivariateRandomForest::Node_cost(y_test_left, inv_cov_y, command)
  cost_right <- MultivariateRandomForest::Node_cost(y_test_right, inv_cov_y, command)

  return (cost - cost_left - cost_right)
}


GetImportanceMeasuresForSingleTree <- function(tree, test_X, test_Y, inv_cov_y, command) {
  num_vars <- ncol(test_X)

  all_node_measures <- array(0, num_vars)

  CalculateSplitMeasures <- function(node_index, sub_test_X, sub_test_Y) {
    node_split <- tree[[node_index]]

    if (!is.null(node_split) && !is.null(node_split$Feature_number)) {
      split_var <- node_split$Feature_number
      split_point <- node_split$Threshold

      split_res <- SplitDataset(sub_test_X, sub_test_Y, split_var, split_point)

      spl_measure <- SplitImprovementMeasure(split_res$left_y, split_res$right_y, inv_cov_y, command)

      all_node_measures[[split_var]] <<- all_node_measures[[split_var]] + spl_measure

      CalculateSplitMeasures(node_split[[5]][[1]], split_res$left_x, split_res$left_y)

      CalculateSplitMeasures(node_split[[5]][[2]], split_res$right_x, split_res$right_y)
    }
  }

  CalculateSplitMeasures(1, test_X, test_Y)

  # aggregate node results with mean
  return (all_node_measures)
}

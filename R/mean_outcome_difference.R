#' Mean Outcome Difference Importance Function
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
#' @param alpha_threshold threshold for split significant testing. If default value of 0 is specified,
#' all the node splits will contribute to result, otherwise only those splits with improvement greater
#' than 1-alpha critical value of an f-statistic do. 
#'
#' @return Vector of size N x 1
#' @export
#'
#' @examples
#' X = matrix(runif(50*5), 50, 5)
#' Y = matrix(runif(50*2), 50, 2)
#' MeanOutcomeDifference(X, Y)
MeanOutcomeDifference <- function(X, Y, sample_size=trunc(nrow(X) * 0.8), num_trees=100, m_feature=ncol(X), min_leaf=10, alpha_threshold) {
  if (ncol(Y) == 1) {
    command = 1
  } else {
    command = 2
  }

  critical_f = qf( 1-alpha_threshold, ncol(Y), nrow(Y)-ncol(Y)-2)
  
  treeMeasures <- array(0,dim=c(num_trees, ncol(X), ncol(Y)))

  sapply(1:num_trees, function(tree_index){
    train_index <- sample(nrow(X), sample_size)
    train_X <- X[train_index,]
    train_Y <- Y[train_index,]

    inv_cov_y = solve(stats::cov(train_Y))
    tree <- MultivariateRandomForest::build_single_tree(train_X, train_Y, m_feature, min_leaf, inv_cov_y, command)

    test_index <- setdiff(1:nrow(X), train_index)
    test_X <- X[test_index, ]
    test_Y <- Y[test_index, ]

    res <- MeanOutcomeDifferenceForSingleTree(tree, test_X, test_Y, command, critical_F)
    treeMeasures[tree_index, ,] <<- array(res, c(1, dim(res)))
  })

  # aggregate tree results with mean
  result <- array(apply(treeMeasures, 1, mean), c(ncol(X), ncol(Y)))
  return (result)
}

MeanOutcomeMeasure <- function(y_test_left, y_test_right) {
  res <- (abs(apply(y_test_left, 2, mean) - apply(y_test_right, 2, mean)))
  res[is.nan(res)] <- 0
  return (res)
}


MeanOutcomeDifferenceForSingleTree <- function(tree, test_X, test_Y, command, critical_f) {
  num_vars <- ncol(test_X)
  dim_y <- ncol(test_Y)

  predict_test_array <- MultivariateRandomForest::single_tree_prediction(tree, X_test=test_X, Variable_number=dim_y)
  var_covar_parentnode <- stats::cov((test_Y - predict_test_array))

  all_node_measures <- matrix(0, num_vars, ncol(test_Y))
  node_measure_counts <- matrix(0, num_vars)

  CalculateSplitMeasures <- function(node_index, sub_test_X, sub_test_Y) {
    node_split <- tree[[node_index]]

    if (!is.null(node_split) && !is.null(node_split$Feature_number)) {
      split_var <- node_split$Feature_number
      split_point <- node_split$Threshold

      split_res <- SplitDataset(sub_test_X, sub_test_Y, split_var, split_point)

      spl_measure <- MeanOutcomeMeasure(split_res$left_y, split_res$right_y)

      if (!is.inf(critical_f)) {
        n_left <- length(split_res$left_y)
        n_right <- length(split_res$right_y)
        mean_leftnode <- apply(split_res$left_y, 2, mean)
        mean_rightnode <- apply(split_res$right_y, 2, mean)

        hotelling_t2 = (n_left * n_right) / (n_left + n_right) * (
          t(mean_leftnode - mean_rightnode) %*% MASS::ginv(var_covar_parentnode) %*% (mean_leftnode - mean_rightnode)
        )
        f_stat <- (n_left + n_right - dim_y - 1) * (hotelling_t2)^2 / (dim_y * (n_left + n_right - 2))
        if (!is.nan(f_stat) && f_stat > critical_f) {
          all_node_measures[split_var, ] <<- all_node_measures[split_var, ] + spl_measure
          node_measure_counts[[split_var]] <<- node_measure_counts[[split_var]] + 1
        }
      } else {
        all_node_measures[split_var, ] <<- all_node_measures[split_var, ] + spl_measure
        node_measure_counts[[split_var]] <<- node_measure_counts[[split_var]] + 1
      }

      CalculateSplitMeasures(node_split[[5]][[1]], split_res$left_x, split_res$left_y)

      CalculateSplitMeasures(node_split[[5]][[2]], split_res$right_x, split_res$right_y)
    }
  }

  CalculateSplitMeasures(1, test_X, test_Y)

  return (all_node_measures)
}

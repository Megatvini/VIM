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
#' @param alpha_threshold threshold for split significant testing. If default value of 0 is specified,
#' all the node splits will contribute to result, otherwise only those splits with improvement greater
#' than 1-alpha critical value of an f-statistic do.
#'
#' @return Vector of size N x 1
#' @details The mean split improvement importance function follows directly from Segal (1992) definition of the 
#' mean structure based split function. For each split defined by feature j, it calculates the difference between the within 
#' parent node sum of squares and the within children-nodes (left and right nodes) measured on either training or testing samples. 
#' If feature j is used in splitting M nodes of the tree, the resulting tree-specific importance measure is the sum of the 
#' node-specific differences calculated across all M nodes. The mean split improvement measure for feature j for the multivariate 
#'random forest is the average of the tree-specific measures across all trees in the forest.
#'
#' If the alpha threshold is 0 all the splits defined by feature j will be used in computing the importance measure. The user also has 
#' the option of including only the significant node splits defined by feature j in the calculation of the importance measure.  The 
#' significance of node splits is measured using an F-test. In this case, the user will need to threshold the alpha critical value of 
#' the F-statistic based on the number of outcome variables in the target matrix and the number of left and right node samples 
#' for the given node split.
#' 
#' Segal MR (1992) Tree-structured methods for longitudinal data. J. American Stat. Assoc. 87(418), 407-418.
#'
#' @export
#'
#' @examples
#' X = matrix(runif(50*5), 50, 5)
#' Y = matrix(runif(50*2), 50, 2)
#' MeanSplitImprovement(X, Y)

MeanSplitImprovement <- function(X, Y, sample_size=trunc(nrow(X) * 0.8), num_trees=100, m_feature=ncol(X), min_leaf=10, alpha_threshold=0) {
  if (ncol(Y) == 1) {
    command = 1
  } else {
    command = 2
  }

  critical_f = stats::qf(1-alpha_threshold, ncol(Y), nrow(Y)-ncol(Y) - 2)

  treeMeasures <- sapply(1:num_trees, function(tree_index){
    train_index <- sample(nrow(X), sample_size)
    train_X <- X[train_index,]
    train_Y <- Y[train_index,]

    inv_cov_y = solve(stats::cov(train_Y))
    tree <- MultivariateRandomForest::build_single_tree(train_X, train_Y, m_feature, min_leaf, inv_cov_y, command)

    test_index <- setdiff(1:nrow(X), train_index)
    test_X <- X[test_index, ]
    test_Y <- Y[test_index, ]

    GetImportanceMeasuresForSingleTree(tree, test_X, test_Y, inv_cov_y, command, critical_f)
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


GetImportanceMeasuresForSingleTree <- function(tree, test_X, test_Y, inv_cov_y, command, critical_f) {
  num_vars <- ncol(test_X)
  dim_y <- ncol(test_Y)

  predict_test_array <- MultivariateRandomForest::single_tree_prediction(tree, X_test=test_X, Variable_number=dim_y)
  var_covar_parentnode <- stats::cov((test_Y - predict_test_array))

  all_node_measures <- array(0, num_vars)

  CalculateSplitMeasures <- function(node_index, sub_test_X, sub_test_Y) {
    node_split <- tree[[node_index]]

    if (!is.null(node_split) && !is.null(node_split$Feature_number)) {
      split_var <- node_split$Feature_number
      split_point <- node_split$Threshold

      split_res <- SplitDataset(sub_test_X, sub_test_Y, split_var, split_point)

      spl_measure <- SplitImprovementMeasure(split_res$left_y, split_res$right_y, inv_cov_y, command)

      if (!is.infinite(critical_f)) {
        n_left <- length(split_res$left_y)
        n_right <- length(split_res$right_y)
        mean_leftnode <- apply(split_res$left_y, 2, mean)
        mean_rightnode <- apply(split_res$right_y, 2, mean)

        hotelling_t2 = (n_left * n_right) / (n_left + n_right) * (
          t(mean_leftnode - mean_rightnode) %*% MASS::ginv(var_covar_parentnode) %*% (mean_leftnode - mean_rightnode)
        )
        f_stat <- (n_left + n_right - dim_y - 1) * (hotelling_t2)^2 / (dim_y * (n_left + n_right - 2))
        if (!is.nan(f_stat) && f_stat > critical_f) {
          all_node_measures[[split_var]] <<- all_node_measures[[split_var]] + spl_measure
        }
      } else {
        all_node_measures[[split_var]] <<- all_node_measures[[split_var]] + spl_measure
      }
      CalculateSplitMeasures(node_split[[5]][[1]], split_res$left_x, split_res$left_y)
      CalculateSplitMeasures(node_split[[5]][[2]], split_res$right_x, split_res$right_y)
    }
  }

  CalculateSplitMeasures(1, test_X, test_Y)

  # aggregate node results with mean
  return (all_node_measures)
}

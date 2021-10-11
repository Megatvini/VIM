#' Variable Importance Function
#'
#' @param X Training Features
#' @param Y Output Features
#' @param numTrees Number of Trees to generate
#' @param sampleSize Size of random subset for each tree generation
#' @param m_feature Number of randomly selected features considered for a split in each regression tree node, which
#' must be positive integer and less than N (number of input features)
#' @param min_leaf Minimum number of samples in the leaf node. If a node has less than or equal
#' to min_leaf samples, then there will be no splitting in that node and this node
#' will be considered as a leaf node. Valid input is positive integer, which is less
#' than or equal to M (number of training samples)
#' @param Command 1 for univariate Regression Tree (corresponding to RF) and 2 for Multivariate
#' Regression Tree (corresponding to MRF)
#'
#' @return Vector of size N x 1
#' @export
#'
#' @examples
#' X = matrix(runif(50*100), 50, 100)
#' Y = matrix(runif(50*5), 50, 5)
#' m_feature = 5
#' min_leaf = 5
#' n_trees = 100
#' sampleSize = 20
#' Command = 2
#' variableImportance(X, Y, n_trees, sampleSize, m_feature, min_leaf, Command)

variableImportance <- function(X, Y, numTrees, sampleSize, m_feature, min_leaf, Command) {
  treeMeasures <- sapply(1:numTrees, function(treeIndex){
    trainIndex <- sample(nrow(X), sampleSize)
    trainX <- X[trainIndex,]
    trainY <- Y[trainIndex,]

    Inv_Cov_Y = solve(stats::cov(trainY))
    tree <- MultivariateRandomForest::build_single_tree(trainX, trainY, m_feature, min_leaf, Inv_Cov_Y, Command)

    testIndex <- setdiff(1:nrow(X), trainIndex)
    testX <- X[testIndex, ]
    testY <- Y[testIndex, ]

    getImportanceMeasuresForSingleTree(tree, testX, testY, Inv_Cov_Y, Command, splitImprovementMeasureFn)
  })
  # aggregate tree results with mean
  apply(treeMeasures, 1, mean)
}


getImportanceMeasuresForSingleTree <- function(tree, testX, testY, Inv_Cov_Y, Command, splitMeasureFn) {
  numVars <- ncol(testX)

  allNodeMeasures <- list()  # variable index => importance measure
  allNodeMeasures[numVars + 1] <- NULL

  for (nodeSplit in tree) {
    if (is.null(nodeSplit) | is.null(nodeSplit$Feature_number)) {
      break
    }

    YLeft <- testY[nodeSplit$Idx_left, , drop=FALSE]
    YRight <- testY[nodeSplit$Idx_right, , drop=FALSE]
    splMeasure <- splitMeasureFn(YLeft, YRight, Inv_Cov_Y, Command)

    splitVar <- nodeSplit$Feature_number
    allNodeMeasures[[splitVar]] <- c(allNodeMeasures[[splitVar]], splMeasure)
  }

  # aggregate node results with mean
  sapply(1:numVars, function(i){
    if (length(allNodeMeasures[[i]]) == 0) {
      0
    } else {
      mean(allNodeMeasures[[i]])
    }
  })
}

splitImprovementMeasureFn <- function(YTestLeft, YTestRight, Inv_Cov_Y, Command) {
  YTest <- rbind(YTestLeft, YTestRight)
  cost <- MultivariateRandomForest::Node_cost(YTest, Inv_Cov_Y, Command)

  costLeft <- MultivariateRandomForest::Node_cost(YTestLeft, Inv_Cov_Y, Command)
  costRight <- MultivariateRandomForest::Node_cost(YTestRight, Inv_Cov_Y, Command)

  cost - costLeft - costRight
}

outcomeDifferenceSplitImprovementMeasure <- function(YTestLeft, YTestRight, Inv_Cov_Y, Command) {
  # TODO
  1
}

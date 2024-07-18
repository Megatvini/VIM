SplitDataset <- function(sub_test_X, sub_test_Y, split_var, split_point) {
  result <- list()
  if (inherits(sub_test_X[, split_var], "numeric")) {
    left_index <- sub_test_X[, split_var] <= split_point
    right_index <- sub_test_X[, split_var] > split_point

    result$left_x <- sub_test_X[left_index, , drop=FALSE]
    result$left_y <- sub_test_Y[left_index, , drop=FALSE]

    result$right_x <- sub_test_X[right_index, , drop=FALSE]
    result$right_y <- sub_test_Y[right_index, , drop=FALSE]
  } else {
    # TODO split factor features
    # result$left <- dataset
    # result$right <- dataset
  }
  result
}


#' k-nearest neighbors cross-validation
#'
#' This function performs k-nearest neighbors cross-validation test to predict class using given covariates.
#'
#' @param train Input data frame.
#' @param cl True class value of the training data.
#' @param k_nn A integer representing the number of neighbors.
#' @param k_cv A integer representing the number of folds.
#' @keywords k-nearest neighbors
#'
#' @return A list with objects \code{class}, which is a vector that includes
#'   the predicted class for all observations, and \code{cv_err}, which is
#'   a numeric with the cross-validation misclassification error.
#'
#' @import class
#' @importFrom stats predict
#' @importFrom dplyr mutate select filter
#' @import tidyselect
#'
#' @examples
#' peng_clean <- na.omit(my_penguins)
#' my_knn_cv(peng_clean[, 3:6], peng_clean$species, 1, 5)
#'
#' @export
my_knn_cv <- function(train, cl, k_nn, k_cv) {
  n <- nrow(train)
  # Split data in k_cv parts, randomly
  fold <- sample(rep(1:k_cv, length = n))
  # Create a data frame that have both covariates and true class value of training data
  data_train <- data.frame("x" = train, "y" = cl, "split" = fold)
  # A vector to store CV misclassification error in each iteration
  pred_err <- rep(NA, k_cv)
  for (i in 1:k_cv) {
    train <- data_train %>% dplyr::filter(split != i)
    test <- data_train %>% dplyr::filter(split == i)
    # Find covariates in training and testing data sets
    cov_train <- train %>% select(starts_with("x."))
    cov_test <- test %>% select(starts_with("x."))
    # Train models
    pred_test <- knn(train = cov_train, test = cov_test, cl = train$y, k = k_nn)
    # Calculate CV misclassification error
    pred_err[i] <- sum(pred_test != test$y) / nrow(test)
  }
  # Store covariates in the full data set
  data_train_cov <- data_train %>% select(starts_with("x."))
  class <- knn(train = data_train_cov, test = data_train_cov,
               cl = data_train$y, k = k_nn)
  cv_err <- mean(pred_err)
  return(list(class, cv_err))
}

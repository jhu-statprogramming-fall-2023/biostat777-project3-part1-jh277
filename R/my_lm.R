#' Linear regression model
#'
#' This function fits a linear regression model to given data.
#'
#' @param my_formula A \code{formula} class object.
#' @param my_data Input data frame.
#' @keywords lm
#'
#' @return Return a table with rows for each coefficient and columns for
#'   \code{Estimate}, \code{Std. Error}, \code{t value}, and \code{p value}.
#'
#' @import tibble
#' @importFrom stats model.frame model.matrix model.response predict pt sd
#' @import knitr
#' @import kableExtra
#' @examples
#' grades_data <- read.csv("https://www.openintro.org/data/csv/gpa.csv")
#' my_lm(gpa ~ studyweek, grades_data)
#'
#' @export
my_lm <- function(my_formula, my_data) {
  # extract the model matrix X
  my_x <- model.matrix(my_formula, data = my_data)
  # extract a model frame object
  my_frame <- model.frame(my_formula, data = my_data)
  # extract the model response Y
  my_y <- model.response(my_frame)
  # calculate beta_hat
  my_beta_hat <- solve((t(my_x) %*% my_x)) %*% t(my_x) %*% my_y
  my_df <- nrow(my_x) - ncol(my_x)
  # calculate sigma square
  my_sigma_square <- sum((my_y - my_x %*% my_beta_hat)^2) / my_df
  my_cov <- my_sigma_square * solve((t(my_x) %*% my_x))
  # calculate standard error
  my_se <- sqrt(diag(my_cov))
  # calculate t-test statistics
  my_t <- (my_beta_hat - 0) / my_se
  # calculate p-value
  my_p_value <- 2 * pt(abs(my_t), lower.tail = FALSE, df = my_df)
  output <- data.frame("Estimate" = my_beta_hat,
                       "Std.Error" = my_se,
                       "t value" = my_t,
                       'p value' = my_p_value)
  kable_styling(kable(output))
}

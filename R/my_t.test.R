#' One sample t test
#'
#' This function evaluates if the value of the given numeric vector is significantly different from the null value (mu).
#'
#' @param x A numeric vector of data.
#' @param alternative A character string specifying the alternative hypothesis,
#'   which only accept `"two.sided"`, `"less"`, or `"greater"`.
#' @param mu A number indicating the null hypothesis value of the mean.
#' @keywords t.test
#'
#' @return A list with elements \code{test_stat} representing the numeric test statistic,
#'   \code{df} representing the degrees of freedom, \code{alternative} representing
#'   the value of the parameter `alternative`, and \code{p_val} representing
#'   the numeric p-value.
#'
#' @importFrom stats pt sd
#' @examples
#' helium_data <- read.csv("https://www.openintro.org/data/csv/helium.csv")
#' my_t.test(x = helium_data$helium, alternative = "two.sided", mu = 20)
#' my_t.test(x = helium_data$helium, alternative = "greater", mu = 20)
#'
#' @export
my_t.test <- function(x,
                      alternative = c("two.sided", "less", "greater"),
                      mu) {
  if (alternative == "two.sided" ||
      alternative == "less" ||
      alternative == "greater") {
    my_se <- sd(x) / sqrt(length(x))
    my_test_stat <- (mean(x) - mu) / my_se
    my_df <- length(x) - 1
    if (alternative == "two.sided") {
      p_val <- 2 * pt(abs(my_test_stat), lower.tail = FALSE, df = my_df)
    } else if (alternative == "less") {
      p_val <- pt(my_test_stat, df = my_df)
    } else {
      p_val <- pt(my_test_stat, lower.tail = FALSE, df = my_df)
    }
    t.test_list <- list("test_stat" = my_test_stat,
                        "df" = my_df,
                        "alternative" = alternative,
                        "p_val" = p_val)
    return(t.test_list)
  } else {
    # throw error message if the input for alternative parameter is not "two.sided", "less", or "greater"
    stop("alternative can only be 'two.sided', 'less' or 'greater'")
  }
}

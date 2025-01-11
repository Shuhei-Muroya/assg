#' Automatically Set glmnet Setting Parameters
#'
#' This function uses a neural network model to find optimal `nlambda` and `thresh` parameters based on input data.
#'
#' @param X Numeric vector of input data.
#' @param size Integer, number of samples to generate for optimization.
#' @param T_hope Numeric, desired threshold time for computation.
#' @param message Logical, whether to display result messages.
#'
#' @return A list containing optimal `nlambda`, `thresh`, pareto front, and pareto data.
#'
#' @import Rcpp
#' @importFrom Rcpp sourceCpp
#' @importFrom readr read_csv
#' @export
auto_settingvalue <- function(X, size = 1000, T_hope = 20, seed=1,message = TRUE,line=TRUE) {
  x_input <- make_input(X)
  x_input <- as.numeric(x_input)
  x_input_matrix <- matrix(rep(x_input, size), ncol = length(x_input), byrow = TRUE)

  # 追加の設定値
  set.seed(seed)
  nlambda <- runif(size, 50, 1900)
  log_thresh <- runif(size, log(1e-9), log(1e-7))
  thresh <- exp(log_thresh)
  x_input_matrix <- cbind(x_input_matrix, nlambda, thresh)
  x_input_matrix <- unname(as.matrix(x_input_matrix))

  results <- apply(x_input_matrix, 1, function(x) {
    myNN_glmnet_cpp(as.numeric(x))
  })

  results <- t(results)
  results <- cbind(results, nlambda, thresh)

  result_return <- cp_pareto_front(results,T_hope,line)

  pareto_data <- result_return$data
  filtered_data <- subset(pareto_data, Pareto == 1 & Time <= T_hope)

  # TimeがT_hope以下で'Coef_Accuracy' が最小の行を選ぶ
  min_accuracy_row <- filtered_data[which.min(filtered_data$Coef_Accuracy), ]

  # 結果を表示
  params_nlambda <- min_accuracy_row$params_nlambda
  params_thresh <- min_accuracy_row$params_thresh

  if (message) {
    cat("params_nlambda:", params_nlambda, "\n")
    cat("params_thresh:", params_thresh, "\n")
  }

  setting_value <- data.frame(
    nlambda = params_nlambda,
    thresh = params_thresh
  )

  result_list <- list(setting_value = setting_value, pareto_front = result_return$pareto_front, pareto_data = pareto_data)
  return(result_list)
}

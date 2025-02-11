#' Automatically select hyperparameters for glmnet
#'
#' This function uses a neural network model to find optimal `nlambda` and `thresh` parameters based on input data.
#'
#' @param X Input matrix.
#' @param size Integer, number of samples to generate pareto front for optimization.
#' @param T_hope A numeric value representing the desired computation time threshold. Default is 20.
#' @param message A logical value. If `TRUE`, messages will be displayed. Default is `TRUE`.
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
  p<-ncol(X)

  # random sampling of hyperparameters for glmnet
  set.seed(seed)
  nlambda <- runif(size, 100, 2*p)
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
  min_accuracy_row <- filtered_data[which.min(filtered_data$Coef_Accuracy), ]

  #result of setting values
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

  result_list <- list(hyperparameters = setting_value, pareto_front = result_return$pareto_front, pareto_data = pareto_data)
  return(result_list)
}

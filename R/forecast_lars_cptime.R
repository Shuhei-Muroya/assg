#' Forecast Lars Computation Time
#'
#' This function predicts the computation time for the lars model based on input data.
#' It also suggests whether to use glmnet or lars based on the predicted computation time.
#'
#' @param X A numeric matrix or vector representing the input data.
#' @param T_hope A numeric value representing the desired computation time threshold. Default is 20.
#' @param message A logical value. If `TRUE`, messages will be displayed. Default is `TRUE`.
#'
#' @details The function takes an input dataset and processes it through a neural network model defined by specific weights and biases loaded from files. The network is designed to predict the computation time for the Lars model.
#'
#' @return A numeric value representing the predicted computation time for the Lars model.
#'
#' @examples
#' # Example usage:
#' # Generate a 1000x700 matrix with random normal values
#' X <- matrix(rnorm(1000 * 700), nrow = 1000, ncol = 700)
#' forecast_time <- forecast_lars_cptime(X, T_hope = 20, message = TRUE)
#'
#' @import RcppArmadillo
#' @import Rcpp
#' @export
forecast_lars_cptime <- function(X, T_hope = 20, message = TRUE) {
  x_input <- make_input(X)
  x_input <- as.numeric(x_input)
  result <- myNN_lars_cpp(x_input)
  pred_time <- result[1]#time
  if (message) {
    cat("The predicted lars computation time is", pred_time, ".","\n")
    if (T_hope < pred_time) {
      cat("It might be better to use 'glmnet'.\n")
    } else {
      cat("It might be better to use 'lars'.\n")
    }
  }
  return(pred_time)
}

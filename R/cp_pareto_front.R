#' Compute and Plot Pareto Front
#'
#' This function identifies the Pareto front in a dataset based on computation time and coefficient accuracy,
#' and generates a plot to visualize the Pareto front.
#'
#' @param data A data frame with columns for time, coefficient accuracy, nlambda, and thresh.
#' @return A list containing a ggplot object for the Pareto front plot and a data frame with Pareto status.
#'
#' @import dplyr
#' @import ggplot2
#' @export
cp_pareto_front <- function(data,T_hope=20,line=TRUE) {
  data <- as.data.frame(data)
  colnames(data)[c(2, 3, 4, 5)] <- c("Time", "Coef_Accuracy", "params_nlambda", "params_thresh")
  data <- data[, c("Time", "Coef_Accuracy", "params_nlambda", "params_thresh")]

  df <- data %>%
    rowwise() %>%
    mutate(Pareto = if_else(any(Coef_Accuracy > data$Coef_Accuracy & Time > data$Time), 0, 1))
  if(line){
    plt_pareto <- ggplot(data = df, aes(x = Coef_Accuracy, y = Time, color = factor(Pareto))) +
      geom_point(alpha = ifelse(df$Pareto == 1, 1, 0.3)) +
      scale_color_manual(values = c("0" = "blue", "1" = "red"), labels = c("0" = "Not Pareto Front", "1" = "Pareto Front")) +
      labs(x = "Coef Accuracy", y = "Time", title = "Pareto Front", color = "Pareto Status") +
      theme(
        legend.position = "inside",
        legend.position.inside = c(0.95, 0.95),
        legend.justification = c("right", "top"),
        legend.background = element_rect(fill = alpha("white", 0.6))
      ) +
      geom_hline(yintercept = T_hope, linetype = "dashed", color = "black", size = 1) +
      annotate("text", x = max(df$Coef_Accuracy), y = T_hope, label = paste("T_hope =", T_hope), hjust = 1.1, vjust = -0.5)
  }else{
    plt_pareto <- ggplot(data = df, aes(x = Coef_Accuracy, y = Time, color = factor(Pareto))) +
      geom_point(alpha = ifelse(df$Pareto == 1, 1, 0.3)) +
      scale_color_manual(values = c("0" = "blue", "1" = "red"), labels = c("0" = "Not Pareto Front", "1" = "Pareto Front")) +
      labs(x = "Coef Accuracy", y = "Time", title = "Pareto Front", color = "Pareto Status") +
      theme(
        legend.position = "inside",
        legend.position.inside = c(0.95, 0.95),
        legend.justification = c("right", "top"),
        legend.background = element_rect(fill = alpha("white", 0.6))
      )
  }



  return(list(pareto_front = plt_pareto, data = df))
}

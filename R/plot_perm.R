#' Plot Variable Importance Scores from Left-Hand Permutation Test
#'
#' Plots the distribution of variable importance scores from the `left_perm` function.
#'
#' @param perm_results A list containing the output of the `left_perm` function, including the `plot_df` data frame.
#' @return A ggplot2 object showing the distribution of variable importance scores.
#' @export
plot_perm <- function(perm_results) {

  # Check if ggplot2 and ggridges are installed
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required but not installed. Please install it to use this function.")
  }
  if (!requireNamespace("ggridges", quietly = TRUE)) {
    stop("Package 'ggridges' is required but not installed. Please install it to use this function.")
  }

  requireNamespace("ggplot2")
  requireNamespace("ggExtra")

  # Extract plot_df from perm_results
  plot_df <- perm_results$plot_df

  # Calculate the average real VI score for each feature
  average_vi_scores <- stats::aggregate(VI_Score ~ Feature, data = subset(plot_df, Type == "Original"), mean)

  # Order the features by the average VI score
  ordered_features <- average_vi_scores$Feature[order(-average_vi_scores$VI_Score)]

  # Convert Feature to a factor with levels ordered by the average VI score
  plot_df$Feature <- factor(plot_df$Feature, levels = ordered_features)

  # Create the plot
  p <- ggplot2::ggplot(plot_df, aes(x = VI_Score, y = Feature, fill = Type)) +
    ggridges::geom_density_ridges() +
    ggplot2::scale_fill_manual(values = c("blue", "red")) +
    ggplot2::labs(
         x = "Variable Importance Score (grf)",
         y = "Feature")

  return(p)
}

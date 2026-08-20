library(corrplot)

find_correlations <- function(df){
  
  # Select only numeric columns
  numeric_df <- df[, sapply(df, is.numeric)]
  
  # Check if enough numeric columns exist
  if(ncol(numeric_df) < 2){
    cat("Need at least two numeric columns.\n")
    return(NULL)
  }
  
  # Compute correlation matrix
  corr_matrix <- cor(numeric_df, use = "complete.obs")
  
  # Display correlation plot
  corrplot(
    corr_matrix,
    method = "color",
    type = "upper",
    tl.col = "black",
    tl.srt = 45
  )
  
  return(corr_matrix)
}
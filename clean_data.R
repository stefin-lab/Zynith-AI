library(dplyr)

clean_data <- function(df){
  
  # Remove duplicate rows
  df <- distinct(df)
  
  # Remove rows with all missing values
  df <- df[rowSums(is.na(df)) != ncol(df), ]
  
  # Convert character columns to factors
  df <- data.frame(lapply(df, function(x){
    if(is.character(x))
      as.factor(x)
    else
      x
  }))
  
  return(df)
}
install.packages("shiny")


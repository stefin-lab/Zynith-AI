clean_dataset <- function(df, method){
  
  # Remove duplicate rows
  df <- unique(df)
  
  numeric_cols <- sapply(df, is.numeric)
  
  for(col in names(df)[numeric_cols]){
    
    if(method == "Mean"){
      df[[col]][is.na(df[[col]])] <- mean(df[[col]], na.rm = TRUE)
      
    } else if(method == "Median"){
      df[[col]][is.na(df[[col]])] <- median(df[[col]], na.rm = TRUE)
      
    } else if(method == "Mode"){
      
      mode_value <- names(sort(table(df[[col]]), decreasing = TRUE))[1]
      df[[col]][is.na(df[[col]])] <- as.numeric(mode_value)
      
    } else if(method == "Remove Rows"){
      
      df <- na.omit(df)
      
    }
  }
  
  return(df)
}
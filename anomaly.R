anomaly_detect <- function(df){
  
  numData <- df[, sapply(df, is.numeric)]
  
  boxplot(
    numData,
    col = "lightblue",
    main = "Anomaly Detection (Boxplot)",
    las = 2
  )
  
}

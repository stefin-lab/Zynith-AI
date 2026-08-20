library(ggplot2)

cluster_plot <- function(df){
  
  numData <- df[, sapply(df, is.numeric)]
  
  km <- kmeans(numData, centers = 3)
  
  plot(
    numData[,1],
    numData[,2],
    col = km$cluster,
    pch = 19,
    xlab = names(numData)[1],
    ylab = names(numData)[2],
    main = "K-Means Clustering"
  )
  
}

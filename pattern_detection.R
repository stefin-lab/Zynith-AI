detect_patterns <- function(df){
  
  # Select only numeric columns
  numData <- df[, sapply(df, is.numeric), drop = FALSE]
  
  result <- ""
  
  # If no numeric columns
  if(ncol(numData) == 0){
    return("❌ No numeric columns found in the dataset.")
  }
  
  # ====================================================
  # CORRELATION ANALYSIS
  # ====================================================
  
  result <- paste0(
    result,
    "📊 CORRELATION ANALYSIS\n",
    "----------------------------\n"
  )
  
  if(ncol(numData) >= 2){
    
    corr <- cor(numData, use = "pairwise.complete.obs")
    
    high <- which(abs(corr) > 0.8 & abs(corr) < 1, arr.ind = TRUE)
    
    shown <- character(0)
    
    if(nrow(high) > 0){
      
      for(i in seq_len(nrow(high))){
        
        r <- high[i,1]
        c <- high[i,2]
        
        key <- paste(sort(c(r,c)), collapse="-")
        
        if(!(key %in% shown)){
          
          shown <- c(shown,key)
          
          result <- paste0(
            result,
            "• ",
            colnames(corr)[r],
            " ↔ ",
            colnames(corr)[c],
            " : ",
            round(corr[r,c],2),
            "\n"
          )
        }
      }
      
    }else{
      
      result <- paste0(
        result,
        "No strong correlations detected.\n"
      )
      
    }
    
  }else{
    
    result <- paste0(
      result,
      "Not enough numeric columns.\n"
    )
    
  }
  
  # ====================================================
  # OUTLIER ANALYSIS
  # ====================================================
  
  result <- paste0(
    result,
    "\n📈 OUTLIER ANALYSIS\n",
    "----------------------------\n"
  )
  
  found <- FALSE
  
  for(col in names(numData)){
    
    values <- numData[[col]]
    
    q1 <- quantile(values,0.25,na.rm=TRUE)
    q3 <- quantile(values,0.75,na.rm=TRUE)
    
    iqr <- q3 - q1
    
    lower <- q1 - 1.5*iqr
    upper <- q3 + 1.5*iqr
    
    outliers <- sum(values < lower | values > upper, na.rm=TRUE)
    
    if(outliers > 0){
      
      found <- TRUE
      
      result <- paste0(
        result,
        "• ",
        col,
        " : ",
        outliers,
        " outliers\n"
      )
      
    }
    
  }
  
  if(!found){
    
    result <- paste0(
      result,
      "No significant outliers detected.\n"
    )
    
  }
  
  # ====================================================
  # MISSING VALUE ANALYSIS
  # ====================================================
  
  result <- paste0(
    result,
    "\n🧹 MISSING VALUE ANALYSIS\n",
    "----------------------------\n"
  )
  
  miss <- colSums(is.na(df))
  
  if(sum(miss) == 0){
    
    result <- paste0(
      result,
      "No missing values found.\n"
    )
    
  }else{
    
    for(i in seq_along(miss)){
      
      if(miss[i] > 0){
        
        result <- paste0(
          result,
          "• ",
          names(miss)[i],
          " : ",
          miss[i],
          " missing values\n"
        )
        
      }
      
    }
    
  }
  
  # ====================================================
  # DATASET SUMMARY
  # ====================================================
  
  result <- paste0(
    result,
    "\n📝 SUMMARY\n",
    "----------------------------\n",
    "Rows : ", nrow(df), "\n",
    "Columns : ", ncol(df), "\n",
    "Numeric Columns : ", ncol(numData), "\n",
    "Analysis completed successfully. ✅"
  )
  
  return(result)
  
}

source("Methods/modindices_train.R")
mod_adj_chisq_cv <- function(baseline.model, data, k = 5, min.mi = 10, alpha = .05, ...){
  
  # Specifying the model:
  model <- baseline.model
  
  # Fit all the data:
  fit <- lavaan::cfa(model, data)
  
  # Obtain OOS MIs:
  oos.pvalue <- oos.pvalue(fit, model, data, k)
  
  while (largest_mi < min.mi) {
    
    
  }
  
  
  
  # Obtaining MIs based on training sets:
  MIs <- modindices_train(fit, model, data, k)
  
  # Arranging the MIs from largest to smallest:
  MIs <- MIs %>% arrange(-mi)
  
  # Obtaining the restricter parameter with the largest MI value:
  largest_mi <- MIs[1, ]
  
  # Specifying a modification to be added to the model:
  mod <- paste(largest_mi[1, 1], largest_mi[1, 2], largest_mi[1, 3], sep = " ")
    
  # Create a test fit:
  fit_test <- lavaan::cfa(model, test)
    
  # Obtain pvalue:
  pvalues[i] <- fitmeasures(fit_test, c("pvalue"))
    
  }
  
  
  




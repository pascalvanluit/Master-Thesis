
###############################

mod_adj_chisq_cv <- function(baseline.model, data, k = 5, min.mi = 10, ...){
  
  # Specifying the model:
  model <- baseline.model
  
        # Space to save p-values of OOS fits:
        pvalues <- rep(0, k - 1)
  
  # Fit all the data:
  fit <- lavaan::cfa(model, data)
  
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
  
  
  




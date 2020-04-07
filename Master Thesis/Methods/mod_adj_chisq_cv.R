source("Methods/oos_pvalue.R")
mod_adj_chisq_cv <- function(baseline.model, data, k = 5, min.mi = 10, alpha = .05, ...){
  
  # Specifying the model:
  model <- baseline.model
  
  # Fit all the data:
  fit <- lavaan::cfa(model, data)

  # Obtain MIs from training set and only accept if OOS fit is significant on average:
  MIs <- modindices_train(fit, model, data, k)
  
  # Arranging the MIs from largest to smallest:
  MIs <- MIs %>% arrange(-mi)
  
  # Extracting the restricted parameter with the largest MI value:
  largest.mi <- MIs[1, ]
  
  # Specifying a modification to be added to the model:
  mod <- paste(largest.mi[1, 1], largest.mi[1, 2], largest.mi[1, 3], sep = " ")
  
  ## Starting the while loop:  
  while(largest.mi[1, 4] > min.mi) {
    
    # Extracting the modification to be added to the model:
    mod <- paste(MIs[1, 1], MIs[1, 2], MIs[1, 3], sep = " ")
    
    # Adding the modification to the model:
    model <- paste(model, mod, sep = "\n")
    
    # Fitting model to the data
    fit <- lavaan::cfa(model, data, ...)
    
    # Obtaining MI values:
    MIs <- modindices_train(fit, model, data, k)
    
    # Arranging the MIs from largest to smallest:
    MIs <- MIs %>% arrange(-mi)
    
    # Updating largest.mi:
    largest.mi <- MIs[1, ]
    
  }
  
  # Print the final model:
  final.model <- tail(model, 1)
  return(final.model)
  
}
  
  
  
  




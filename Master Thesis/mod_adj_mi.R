mod_adj_mi <- function(baseline.model,data, min.mi = 10, ...){
  
  # Saving baseline model as model:
  model <- baseline.model
  
  # Fitting the model to the data:
  fit <- lavaan::cfa(model, data)
  
  # Obtaing MI values:
  MIs <- lavaan::modindices(fit)
  
  # Wrangling the MIs:
  MIs <- MIs %>%
    arrange(-mi) %>% 
    select(lhs, op, rhs, mi)
  
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
    fit <- lavaan::cfa(model, data)
    
    # Obtaining MI values:
    MIs <- lavaan::modindices(fit)
    
    # Wrangling the MIs:
    MIs <- MIs %>%
      arrange(-mi) %>%
      select(lhs, op, rhs, mi)
    
    # Updating largest.mi:
    largest.mi <- MIs[1, ]
    
    # Name the model:
    # print(tail(model, 1))
    
  }
  
  # Print the final model:
  final.model <- tail(model, 1)
  return(final.model)
  
}

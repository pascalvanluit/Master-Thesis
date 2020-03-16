modindices_train <- function(fit, model, data, k){
  
  # Splitting the dataset into k groups
  n_obs      <- nrow(data)
  select_vec <- rep(1:k, length.out = n_obs)
  data_split <- data %>% mutate(fold = sample(select_vec))
  
  # Obtaining MI values:
  
  # Fitting the model on the full dataset to create a space where train MIs can be saved:
  mi          <- lavaan::modindices(fit, na.remove = FALSE)
  mi[, -1:-3] <- 0
  
  # Loop of fitting model to training set to get MI values:
  for (i in 1:k) {
    
    # Splitting the data:
    train <- data_split %>% filter(fold != i)
    test  <- data_split %>% filter(fold == i)
    
    # Creating train and test fits:
    fit_train <- lavaan::cfa(model, train)
    
    # Obtaining MI values:
    mi_train <- lavaan::modindices(fit_train, na.remove = FALSE)
    mi_train[is.na(mi_train)] <- 0
    
    # Combining the OOS MI values:
    mi[, -1:-3] <- mi[, -1:-3] + mi_train[, -1:-3]
  }
  
  # Fixing up the MI output
  mi[, -1:-3] <- mi[, -1:-3] / k
  
  return(mi)
}

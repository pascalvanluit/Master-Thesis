cv_modindices <- function(fit, model, data, k = 5){
  
  # Splitting the dataset into k groups
  n_obs      <- nrow(data)
  select_vec <- rep(1:k, length.out = n_obs)
  data_split <- data %>% mutate(fold = sample(select_vec))
  
  # Obtaining MI values:
  
  # Fitting the model on the full dataset to create a space where OOS MIs can be saved:
  cv_mi          <- lavaan::modindices(fit, na.remove = FALSE)
  cv_mi[, -1:-3] <- 0
  
  # Loop of fitting model to training set and then to test set to get MI values:
  for (i in 1:k) {
    
    # Splitting the data:
    train <- data_split %>% filter(fold != i)
    test  <- data_split %>% filter(fold == i)
    
    # Creating train and test fits:
    fit_train <- lavaan::cfa(model, train)
    fit_test  <- lavaan::cfa(model, test, start = fit_train, do.fit = FALSE)
    
    # Obtaining MI values:
    mi_test <- lavaan::modindices(fit_test, na.remove = FALSE)
    mi_test[is.na(mi_test)] <- 0
     
    # Combining the OOS MI values:
    cv_mi[, -1:-3] <- cv_mi[, -1:-3] + mi_test[, -1:-3]
  }
  
  # Fixing up the MI output
  cv_mi[, -1:-3] <- cv_mi[, -1:-3] / k
  
  return(cv_mi)
}

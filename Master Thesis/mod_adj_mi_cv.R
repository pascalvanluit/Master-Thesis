set.seed(88)

mod_adj_mi_cv <- function(baseline.model, data, k = 5, min.mi = 10){

  # Saving the baseline.model as model:
  model <- baseline.model
  
  # Splitting the dataset into k groups
  n_obs      <- nrow(data)
  select_vec <- rep(1:k, length.out = n_obs)
  data_split <- data %>% mutate(fold = sample(select_vec))
  
  # Obtaining MI values:
  
    # Fitting the model on the full dataset to create a space where OOS MIs can be saved:
    fit_full <- lavaan::cfa(model, data)
    mi       <- lavaan::modindices(fit_full)
    mi       <- mi %>% select(lhs, op, rhs, mi)
    mi[, 4]  <- 0
    cv_mi    <- mi
    
  
    # Loop of fitting model to training set and then to test set to get MI values:
    for (i in 1:k) {
    
      # Splitting the data:
      train <- data_split %>% filter(fold != i)
      test  <- data_split %>% filter(fold == i)
    
      # Creating train and test fits:
      fit_train <- lavaan::cfa(model, train)
      fit_test  <- lavaan::cfa(model, test, start = fit_train, do.fit = FALSE)
    
      # Obtaining MI values:
      mi_test <- lavaan::modindices(fit_test)
    
        # Wrangling MI output:
        mi_test <- mi_test %>% 
          select(lhs, op, rhs, mi)
    
      # Combining the OOS MI values:
      cv_mi[, 4] <- mi_test[, 4]
    
      # Fixing up the MI output
      cv_mi <- cv_mi %>%
        mutate_if(is.numeric, round, 3) %>% 
        mutate(mi = mi / k) %>% 
        select(lhs, op, rhs, mi) %>% 
        arrange(-mi)
    
      # Checking output:
      # return(cv_mi)
  }
  
  # Obtaining the restricter parameter with the largest MI value:
  largest_mi <- cv_mi[1, ]
  
  # Specifying a modification to be added to the model:
  mod <- paste(largest_mi[1, 1], largest_mi[1, 2], largest_mi[1, 3], sep = " ")
  
  # While loop 
  while (largest_mi[1, 4] > min.mi) {
    
    # Obtaining the modification to be added to the model:
    mod <- paste(cv_mi[1, 1], cv_mi[1, 2], cv_mi[1, 3], sep = " ")
    
    # Adding the modification to the model:
    model <- paste(model, mod, sep = "\n")
    
    # Fitting the model on the full dataset to create a space where OOS sample mod indices can be saved:
    fit_full <- lavaan::cfa(model, data)
    mi       <- lavaan::modindices(fit_full)
    mi       <- mi %>% select(lhs, op, rhs, mi)
    mi[, 4]  <- 0
    cv_mi    <- mi
    
    # Loop of fitting the modified model to training set and test set to get MIs:
    for (i in 1:k) {
      
      # Splitting the data:
      train <- data_split %>% filter(fold != i)
      test  <- data_split %>% filter(fold == i)
    
      # Creating train and test fits:
      fit_train <- lavaan::cfa(model, train)
      fit_test  <- lavaan::cfa(model, test, start = fit_train, do.fit = FALSE)
    
      # Obtaining MI values:
      mi_test <- lavaan::modindices(fit_test)
    
      # Wrangling MI output:
      mi_test <- mi_test %>% 
        select(lhs, op, rhs, mi)
    
      # Combining the OOS MI values:
      cv_mi[, 4] <- mi_test[, 4]
    
      # Fixing up the MI output
      cv_mi <- cv_mi %>%
        mutate_if(is.numeric, round, 3) %>% 
        mutate(mi = mi / k) %>% 
        select(lhs, op, rhs, mi) %>% 
        arrange(-mi)
      
    }
    
    # Updating largest_mi:
    largest_mi <- cv_mi[1, ]
  
  }

  # Print the final model:
  final_model <- tail(model, 1)
  return(final_model)
  
}


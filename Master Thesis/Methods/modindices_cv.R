modindices_cv <- function(fit, model, data, k, ...){

#######################################  
# Splitting the dataset into k groups #
#######################################
  
  n_obs      <- nrow(data)
  select_vec <- rep(1:k, length.out = n_obs)
  data_split <- data %>% mutate(fold = sample(select_vec))
  
#######################
# Obtaining MI values #
#######################
  
  # Fitting the model on the full dataset to create a space where OOS MIs can be saved:
  cv_mi          <- try(lavaan::modindices(fit, na.remove = FALSE), silent = TRUE)
  if (inherits(cv_mi, "try-error"))
    return(cv_mi <- data.frame(lhs = NA, op = '=', rhs = NA, mi = -1))
                        
  cv_mi[, -1:-3] <- 0
  
  # Loop of fitting model to training set and then to test set to get MI values:
  for (i in 1:k) {
    
    # Splitting the data:
    train <- data_split %>% filter(fold != i)
    test  <- data_split %>% filter(fold == i)
    
    # Creating train and test fits:
    fit_train <- try(lavaan::cfa(model, train, ...), silent = TRUE)
    if (inherits(fit_train, "try-error"))
      {
      ifelse(is.atomic(fit_train) == TRUE, fit_train <- NA, fit_train)
    }
    fit_test  <- lavaan::cfa(model, test, start = fit_train, do.fit = FALSE, ...)
    #   try(lavaan::cfa(model, test, start = fit_train, do.fit = FALSE, ...), silent = TRUE)
    # if (inherits(fit_test, "try-error"))
    #   {
    #   ifelse(is.atomic(fit_test) == TRUE, fit_test <- list(NA), fit_test)
    # }
    
    # Obtaining MI values:
    mi_test <- try(lavaan::modindices(fit_test, na.remove = FALSE), silent = TRUE)
    if (inherits(mi_test, "try-error"))
      return(cv_mi <- data.frame(lhs = NA, op = '=', rhs = NA, mi = -1))
    mi_test[is.na(mi_test)] <- 0
     
    # Combining the OOS MI values:
    cv_mi[, -1:-3] <- cv_mi[, -1:-3] + mi_test[, -1:-3]
  }
  
  # Fixing up the MI output
  cv_mi[, -1:-3] <- cv_mi[, -1:-3] / k
  
  return(cv_mi)
  
}

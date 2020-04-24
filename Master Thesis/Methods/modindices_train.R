modindices_train <- function(fit, model, data, k, alpha, ...){

  alpha <- alpha
  
#######################################
# Splitting the dataset into k groups #
#######################################
  
  n_obs      <- nrow(data)
  select_vec <- rep(1:k, length.out = n_obs)
  data_split <- data %>% mutate(fold = sample(select_vec))

  
#######################
# Obtaining MI values #
#######################
  
  # Fitting the model on the full dataset to create a space where train MIs can be saved:
  fit        <- try(lavaan::cfa(model, data, optim.force.converged = TRUE, ...), silent = TRUE)
  if (inherits(fit, "try-error"))
    return(mi <- data.frame(lhs = NA, op = '=', rhs = NA, mi = -1))
  chisq      <- lavaan::fitmeasures(fit, c("chisq"))
  chisq      <- 0
  
  mi          <- try(lavaan::modindices(fit, na.remove = FALSE), silent = TRUE)
  if (inherits(mi, "try-error"))
    return(mi <- data.frame(lhs = NA, op = '=', rhs = NA, mi = -1))
  mi[, -1:-3] <- 0
  
  # Loop of fitting model to training set to get MI values:
  for (i in 1:k) {
    
    # Splitting the data:
    train <- data_split %>% filter(fold != i)
    
    # Creating train and test fits:
    fit_train <- try(lavaan::cfa(model, train, optim.force.converged = TRUE, ...), silent = TRUE)
    if (inherits(fit_train, "try-error"))
    {
      ifelse(is.atomic(fit_train) == TRUE, fit_train <- NA, fit_train)
    }
    
    # Obtaining MI values:
    mi_train <- try(lavaan::modindices(fit_train, na.remove = FALSE), silent = TRUE)
    if (inherits(mi_train, "try-error"))
      return(cv_mi <- data.frame(lhs = NA, op = '=', rhs = NA, mi = -1))
    mi_train[is.na(mi_train)] <- 0
    
    # Combining the MI values:
    mi[, -1:-3] <- mi[, -1:-3] + mi_train[, -1:-3]
  }
  
  # Finding the average MI:
  mi[, -1:-3] <- mi[, -1:-3] / k
  
  # Specify modification to be added to the model:
  mi         <- mi %>% arrange(-mi)
  largest_mi <- mi[1, ]
  mod        <- paste(largest_mi[1, 1], largest_mi[1, 2], largest_mi[1, 3], sep = " ")
  model      <- paste(model, mod, sep = "\n")

######################
# Obtaining OOS fits #
######################  

  # Loop for finding average chi-square fit on test sets:
  for (i in 1:k) {
    
    # Obtaining test sets:
    test <- data_split %>% filter(fold == i)
    
    # fitting the model to the test set:
    fit_test <- try(lavaan:::cfa(model, test, optim.force.converged = TRUE, ...), silent = TRUE)
    if (inherits(fit_test, "try-error"))
      return(cv_mi <- data.frame(lhs = NA, op = '=', rhs = NA, mi = -1))
    
    # Obtaining the pvalue (significance) of chi-square fit measure:
    chisq_test <- lavaan::fitmeasures(fit_test, c("chisq"))
    chisq      <- chisq + chisq_test
    
  }
  
  # Obtaining average chi-square
  chisq <- chisq / k 

  # Obtaining the degrees of freedom:
  df <- fitmeasures(fit_train, c("df"))
  
  # Creating mi_stop; this makes the largest_mi -1 so that no more modifications are added:
  mi_stop <- data.frame(lhs = NA, op = '=', rhs = NA, mi = -1)
  
  # Return MIs:
  pvalue <- pchisq(chisq, df)
  ifelse(1 - pvalue < alpha, return(mi), return(mi_stop))
  
  
}

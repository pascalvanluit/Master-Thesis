modindices_train <- function(fit, model, data, k){

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
  fit         <- lavaan::cfa(model, data)
  pvalue      <- lavaan::fitmeasures(fit, c("pvalue"))
  pvalue      <- 0
  
  mi          <- lavaan::modindices(fit, na.remove = FALSE)
  mi[, -1:-3] <- 0
  
  # Loop of fitting model to training set to get MI values:
  for (i in 1:k) {
    
    # Splitting the data:
    train <- data_split %>% filter(fold != i)
    
    # Creating train and test fits:
    fit_train <- lavaan::cfa(model, train)
    
    # Obtaining MI values:
    mi_train <- lavaan::modindices(fit_train, na.remove = FALSE)
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
    fit_test <- lavaan::cfa(model, test, optim.force.converged = TRUE)
    
    # Obtaining the chi-square fit measure:
    pvalue_test <- lavaan::fitmeasures(fit_test, c("pvalue"))
    pvalue      <- pvalue + pvalue_test
    
  }
  
  # Obtaining average chi-square
  pvalue <- pvalue / k 
  
  # Creating mi_stop; this makes the largest_mi -1 so that no more modifications are added:
  mi_stop <- data.frame(lhs = NA, op = '=', rhs = NA, mi = -1)
  
  ifelse(pvalue < .5, return(mi), return(mi_stop))
  
  
  # p value OR chisq as criterion for adding mod or not.
  # How do i make the chisq value a criterion? need to define how it is used as a cutoff in the while function.
  
}

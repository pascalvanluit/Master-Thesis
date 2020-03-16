mod_adj_chisq_cv <- function(baseline.model, data, k = 5, min.mi = 10, ...){
  
  # Splitting the data into k groups:
  n_obs <- nrow(data)
  group <- rep(1:k, length.out = n_obs)
  data  <- data %>% mutate(folds = sample(group))
  
  # Saving the baseline model as model:
  model <- baseline.model
  
  # Space to save p-values of OOS fits:
  pvalues <- rep(0, k - 1)
  
  # Obtaining a modification to be added to the model:
  MIs <- modindices_train(fit, model, data, k)
  
  # Arranging the MIs from largest to smallest:
  MIs <- MIs %>% arrange(-mi)
  
  # Extracting the modification with the largest MI:
  largest.mi <- MIs[1, ]
  
  # Specifying a modification to be added to the model:
  mod <- paste(largest.mi[1, 1], largest.mi[1, 2], largest.mi[1, 3], sep = " ")
  
  ############
  # FOR LOOP #
  ############
  for (i in 1:k) {
    
    # split into train and valid set:
    train <- data %>% filter(folds != i)
    valid <- data %>% filter(folds == i)
    
    # Fitting model on training set:
    fit_train_i <- lavaan::cfa(model, train)
    
    # Finding fit of that model on the test set:
    fit_test_i <- lavaan:::lavaan(data = valid,
                                  auto.fix.first = TRUE, auto.var = TRUE,
                                  do.fit = FALSE, start = fit_train_i,
                                  test = "standard",
                                  optim.force.converged = TRUE,
                                  model.type = "cfa")
    
    # Obtaining the model fit measures:
    pvalues[i] <- lavaan::fitmeasures(fit_test_i, c("pvalue"))
    
  }
  
  # Vector to save p-values in:
  pvalues <- rep(0, k)
  
  # p
  mean_p <- mean(pvalues)
  
  ###################
  # WHILE FUNCTTION #
  ###################
  while (mean_p <= .05) {
    
    for (i in 1:k) {
      
      # split into train and valid set:
      train <- data %>% filter(folds != i)
      valid <- data %>% filter(folds == i)
      
      # Fitting model on training set:
      fit_train_i <- lavaan::cfa(model, train)
      
      # Finding fit of that model on the test set:
      fit_test_i <- lavaan:::lavaan(data = valid,
                                    auto.fix.first = TRUE, auto.var = TRUE,
                                    do.fit = FALSE, start = fit_train_i,
                                    test = "standard",
                                    optim.force.converged = TRUE,
                                    model.type = "cfa")
      
      # Obtaining the model fit measures:
      pvalues[i] <- lavaan::fitmeasures(fit_test_i, c("pvalue"))
      
    }
    
    # Take mean of p values found in test set:
    mean_p <- mean(pvalues)
    
    # 
    
  }

  return(final_model)
  
}


###############################

mod_adj_chisq_cv <- function(baseline.model, data, k = 5, min.mi = 10, ...){
  
  # Splitting the data into k groups:
  n_obs <- nrow(data)
  group <- rep(1:k, length.out = n_obs)
  data_split  <- data %>% mutate(folds = sample(group))
  
  # Specifying the model:
  model <- baseline.model
  
  # Space to save p-values of OOS fits:
  pvalues <- rep(0, k - 1)
  
  # Fitting the model to the whole dataset:
  fit <- lavaan::cfa(model, data)
  
  # Fitting the model on the full dataset to create a space where MIs can be saved:
  mi          <- lavaan::modindices(fit, na.remove = FALSE)
  mi[, -1:-3] <- 0
  
  # Loop of getting mean MI values based on different training sets:
  for (i in 1:k) {
    
    # Splitting the data:
    train <- data_split %>% filter(folds != i)
    test  <- data_split %>% filter(folds == i)
    
    # Create training fit:
    fit_train <- lavaan::cfa(model, train)
    
    # Obtain MI values:
    mi_train <- lavaan::modindices(fit_train)
    mi_train[is.na(mi_train)] <- 0
    
    # Combining the MI values:
    mi[, -1:-3] <- mi[, -1:-3] + mi_train[, -1:-3]
    
# Need a space here for getting the pvalues found in the test sets:
    
    # Create a test fit:
    fit_test <- lavaan::cfa(model, test)
    
    # Obtain pvalue:
    pvalues[i] <- fitmeasures(fit_test, c("pvalue"))
    
  }
  
  # Fixing up the MI output
  cv_mi[, -1:-3] <- cv_mi[, -1:-3] / k
  
}



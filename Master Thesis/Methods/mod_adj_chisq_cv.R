mod_adj_chisq_cv <- function(baseline.model, data, k = 5, ...){
  
  # Splitting the data into k groups:
  n_obs <- nrow(data)
  group <- rep(1:k, length.out = n_obs)
  data  <- data %>%
    mutate(folds = sample(group))
  
  # Saving the baseline model as model:
  model <- baseline.model
  
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
  p <- mean(pvalues)
  
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

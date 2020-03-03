mod_adj_mi_cv <- function(baseline.model, data, k = 5){
  
  # Saving the baseline.model as model:
  model <- baseline.model
  
  # Splitting the dataset into k groups
  n_obs      <- nrow(data)
  select_vec <- rep(1:k, length.out = n_obs)
  data_split <- data %>% mutate(fold = sample(select_vec))
  
  # Fitting the model to the full dataset and gettings MIs:
  fit_full <- lavaan::cfa(model, data)
  mi <- lavaan::modindices(fit_full)
  
  # Preparing cross-validated MIs data frame:
  cv_mi <- mi
  cv_mi[, -1:-3] <- 0
  
  # Loop for obtaining CV MIs:
  for (i in 1:k) {
    
    # Splitting the data:
    train <- data_split %>% filter(fold != i)
    test  <- data_split %>% filter(fold == i)
    
    # Creating train and test fits:
    fit_train <- lavaan::cfa(model, train)
    fit_test  <- lavaan::cfa(model, test, start = fit_train, do.fit = FALSE)
    
    # MIs obtained from test set:
    cv_mi[, -1:-3] <- cv_mi[, -1:-3] + lavaan::modindices(fit_test)[, -1:-3]
    
    # Obtaining the mean mi values:
    #mean_cv_mi <- cv_mi / k
    
  }
  
  print(cv_mi)
  
}










mi_cv_kfold <- function(baseline.model, data, k = 5){
  
  # Saving the baseline.model as model:
  model <- baseline.model
  
  ## Setting up the dataset to be split into k folds:
  
  # Adding a column with assignment for each observation:
  n.obs      <- nrow(data)
  select.vec <- rep(1:k, length.out = n.obs)
  data.split <- data %>% mutate(fold = sample(select.vec))
  
  # Create output vector for in the validation set:
  fit.valids <- rep(0, k)
  
  ############
  # FOR LOOP #
  ############
  
  ## Fitting the data to a the different training sets:
  for (i in 1:k) {
    
    # Splitting the data in train and validation set
    train <- data.split %>% filter(fold != i)
    valid <- data.split %>% filter(fold == i)
    
    # Fitting the baseline model on the training set:
    fit.train.i <- lavaan::cfa(model, train)
    
    # Finding which modification to add based on the training portion:
    MIs <- lavaan::modindices(fit.train.i)
    
    # Wrangling the MIs:
    MIs <- MIs %>% arrange(-mi) %>% select(lhs, op, rhs, mi)
    
    # Specifying a modification to be added to the model:
    mod <- paste(MIs[1, 1], MIs[1, 2], MIs[1, 3], sep = " ")
    
    # Fitting the baseline model to the validation set:
    fit.valid.1.i <- lavaan::cfa(model, valid)
    
    # Adding a modification to the model:
    mod.model <- paste(model, mod, sep = "\n")
    
    # Find the fit of the modified model on the validation set:
    fit.valid.2.i <- lavaan::cfa(mod.model, valid)
    
    # Seeing whether this improves the fit:
    chisq.diff.i <- lavaan::lavTestLRT(fit.valid.1.i, fit.valid.2.i)
    
    # Assigning whether fit is significantly better or not:
    ifelse(chisq.diff.i$`Pr(>Chisq)`[2] < .05, sign <- TRUE, sign <- FALSE)
    
    # Finding the mean p-value:
    mean.chisq.diff <- mean(chisq.diff.i)
    
    # Creating chisq.diff object:
    # mean.chisq.diff <- list(`Pr(>Chisq)` = 0)
    
    ###################
    # WHILE FUNCTTION #
    ###################
    while (mean.chisq.diff$`Pr(>Chisq)` < .05) {
      
      # Extracting a modification to be added to the model:
      mod <- paste(MIs[1, 1], MIs[1, 2], MIs[1, 3], sep = " ")
      
      # Adding a modification to the model:
      model <- paste(model, mod, sep = "\n")
      
      # Fitting the model to the validation set:
      fit.valid.2.i <- lavaan::cfa(model, valid)
      
      # Obtaining chisq.diffs and finding the mean:
      fit.valids[i] <- lavaan::lavTestLRT(fit.valid.1.i, fit.valid.2.i)
      mean.chisq.diff <- mean(fit.valids[i])
      
      # Fitting the model to the training set:
      fit.train <- lavaan::cfa(model, train)
      
      # Obtaining new MI values:
      MIs <- lavaan::modindices(fit.train)
      
      # Wrangling the MIs:
      MIs <- MIs %>% arrange(-mi) %>% select(lhs, op, rhs, mi)
      
      # Print the final model:
      print(model)
      
    } 
    
    
    # # Adding the modification to the model:
    # model <- paste(model, mod, sep = "\n")
    # 
    # # Fitting the modified model on the valid set:
    # fit_valid_2 <- lavaan::cfa(model, valid)
    # 
    # # Checking the chisq.diff in the validation set:
    # chisq.diff[i] <- lavaan::lavTestLRT(fit_valid_1_i, fit_valid_2)
    # 
    # # Getting a mean of the chisq.diff's
    # mean.chisq.diff <- mean(chisq.diff[i])
    
    # # Calculate the model fit in the validation set:
    # fit_measures_valids <- lavaan::fitmeasures(fit_valid, c("chisq", "df", "pvalue"))
    # 
    # # Find the average of the fits in the validation sets:
    # fit_valids[i] <- mean(fit_measures_valids)
    # 
    # #Print the final model:
    # print(model)
    
  }
  
  # return()
  
}
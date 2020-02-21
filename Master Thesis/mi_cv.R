mi_cv <- function(baseline.model, data, split, ...){
  
  # Adding a column to the dataset to allow a train-validation split:
  data <- data %>% 
    mutate(split = rep(c("train", "test"),
                       times = c(round(split * nrow(data)), round((1 - split) * nrow(data)))))
  
  # Making a separate training set:
  train <- data %>%
    filter(split == "train") %>% 
    select(-split)
  
  test <- data %>%
    filter(split == "test") %>% 
    select(-split)
  
  # Saving baseline model as model:
  model <- baseline.model
  
  ## Fitting the baseline model to the training set:
  fit.train <- lavaan::cfa(model, train, ...)
  
  # Obtaining MI values according to the training set:
  MIs <- lavaan::modindices(fit.train)
  
  # Wrangling the MIs:
  MIs <- MIs %>%
    arrange(-mi) %>% 
    select(lhs, op, rhs, mi)
  
  # Specifying a modification to be added to the model:
  mod <- paste(MIs[1, 1], MIs[1, 2], MIs[1, 3], sep = " ")
  
  ## Fitting the baseline model to the test set:
  fit.test.1 <- lavaan::cfa(model, test, ...)
  
  # Creating chisq.diff object: ???????????????????????????????????????????
  # chisq.diff <- lavaan::lavTestLRT(fit.train, fit.test.1)
  chisq.diff <- list(`Pr(>Chisq)` = 0)
  
  ###################
  # WHILE FUNCTTION #
  ###################
  while(chisq.diff$`Pr(>Chisq)` < .05){
    
    # Extracting a modification to be added to the model:
    mod <- paste(MIs[1, 1], MIs[1, 2], MIs[1, 3], sep = " ")
    
    # Adding modification to the model:
    model <- paste(model, mod, sep = "\n")
    
    # Fitting the model to the test set:
    fit.test.2 <- lavaan::cfa(model, test)
    
    # Checking the chisq.diff in the test set:
    chisq.diff <- lavaan::lavTestLRT(fit.test.1, fit.test.2) 
    
    # Fitting the model to the train set:
    fit.train <- lavaan::cfa(model, train)
    
    # Obtaining new MI values:
    MIs <- lavaan::modindices(fit.train)
    
    # Wrangling the MIs
    MIs <- MIs %>%
      arrange(-mi) %>% 
      select(lhs, op, rhs, mi)
    
    # Print the final model
    print(model)
    
  }
  
}

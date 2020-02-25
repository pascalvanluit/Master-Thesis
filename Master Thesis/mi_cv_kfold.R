mi_cv_kfold <- function(baseline.model, data, k = 5){
  
  # Saving the baseline.model as model:
  model <- baseline.model
  
## Setting up the dataset to be split into k folds:
  
    # Adding a column with assignment for each observation:
    n_obs      <- nrow(data)
    select_vec <- rep(1:k, length.out = n_obs)
    data_split <- data %>% mutate(fold = sample(select_vec))
    
    # Create output vector for in the validation set:
    fit_valids <- rep(0, k)
    
## Fitting the data to a the different training sets:
  for (i in 1:k) {
    
    # Splitting the data in train and validation set
    train <- data_split %>% filter(fold != i)
    valid <- data_split %>% filter(fold == i)
    
    # Fitting a model on the training set:
    fit_train_i <- lavaan::cfa(model, train)
    
    # Fitting the model to the validation set:
    # fit_valid_i <- lavaan::cfa(model, valid)
    
    # Finding which modification to added based on the training portion:
    MIs <- lavaan::modindices(fit_train_i)
    
      # Wrangling the MIs:
      MIs <- MIs %>% arrange(-mi) %>% select(lhs, op, rhs, mi)
    
    # Specifying a modification to be added to the model:
    mod <- paste(MIs[1, 1], MIs[1, 2], MIs[1, 3], sep = " ")
    
    # Adding the modification to the model:
    model <- paste(model, mod, sep = "\n")
    
    # Fitting the modified model on the valid set:
    fit_valid <- lavaan::cfa(model, valid)
    
    # Calculate the model fit in the validation set:
    fit_measures_valids <- lavaan::fitmeasures(fit_valid[i], c("chisq", "df", "pvalue"))
    
    # Find the average of the fits in the validation sets:
    avg_fit_valids <- mean(fit_measures_valids)
    
    
  }
  return(avg_fit_valids)
}


# Testing out the things I'm adding in the function above to check how and if they work:
n_obs <- nrow(HolzingerSwineford1939)
select_vec <- rep(1:4, length.out = n_obs)
data_split <- HolzingerSwineford1939 %>% mutate(fold = sample(select_vec))

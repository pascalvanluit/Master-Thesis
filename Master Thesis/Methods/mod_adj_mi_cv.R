source("Methods/modindices_cv.R")
mod_adj_mi_cv <- function(data, baseline.model, k = 5, min.mi = 10, ...){

  # Saving the baseline.model as model:
  model <- baseline.model
  
  # Fitting the model to the data:
  fit <- lavaan::cfa(model, data, ...)
  
  # Obtaining MI values:
  MIs <- modindices_cv(fit, model, data, k)
  #   try(modindices_cv(fit, model, data, k), silent = TRUE)
  # if(inherits(MIs, "try-error"))
  #   {
  #   out1 <- list(model = model, fit = fit)
  #   return(out1)
  # }
  
  # Arranging the MIs from largest to smallest:
  MIs <- MIs %>% arrange(-mi)
  
  # Obtaining the restricter parameter with the largest MI value:
  largest_mi <- MIs[1, ]
  
  # Specifying a modification to be added to the model:
  mod <- paste(largest_mi[1, 1], largest_mi[1, 2], largest_mi[1, 3], sep = " ")
  
  # While loop
  while (largest_mi[1, 4] > min.mi) {

    # Obtaining the modification to be added to the model:
    mod <- paste(MIs[1, 1], MIs[1, 2], MIs[1, 3], sep = " ")

    # print("adding a modification")
    
    # Adding the modification to the model:
    model <- paste(model, mod, sep = "\n")

    # Fitting model to the data:
    fit <- try(lavaan::cfa(model, data, ...), silent = TRUE)
    if (inherits(fit, "try-error"))
      {
      ifelse(is.atomic(fit) == TRUE, fit <- list(NA), fit)
      out2 <- list(model = model, fit = fit)
      return(out2)
    }

    # Obtaining MI values:
    MIs <- try(modindices_cv(fit, model, data, k), silent = TRUE)
    if(inherits(MIs, "try-error"))
      {
      out3 <- list(model = model, fit = fit)
      return(out3)
    }

    # Arranging the MIs from largest to smallest:
    MIs <- MIs %>% arrange(-mi)

    # Updating largest_mi:
    largest_mi <- MIs[1, ]

    }

  # Print the final model:
  final.model <- tail(model, 1)
  out4 <- list(model = final.model, fit = fit)
  
  return(out4)
  
}

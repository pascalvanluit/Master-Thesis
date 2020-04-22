mod_no_adj <- function(data, baseline.model, ...){
  
    # Save the baseline model:
    model <- baseline.model
    
    # Fit the model to the data:
    fit <- lavaan::cfa(model, data, ...)
    
    # Prepare the output
    out <- list(model = model, fit = fit)
    
    return(out)
  
}

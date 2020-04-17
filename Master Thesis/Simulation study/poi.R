# Function to extract the estimate of the parameter of interest from a lavaan fit object:
poi <- function(fit){
  x <- parameterestimates(fit)
  x %>% filter(grepl("f1", lhs), grepl("f2", rhs)) %>% select(est)
}
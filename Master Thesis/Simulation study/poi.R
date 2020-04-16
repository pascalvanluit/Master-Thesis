# Function to extract the estimate of the parameter of interest from a parameterestimates object:
poi <- function(x){
  x %>% filter(grepl("f1", lhs), grepl("f2", rhs)) %>% select(est)
}
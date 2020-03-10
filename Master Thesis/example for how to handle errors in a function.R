# handle errors in myfunction for programming in outer function

myfunction <- function() {
  if (runif(1) > 0.5)
    stop("hello this is an error")
  
  "ok this is good"
}

mi_cv_adj <- function() {
  result <- try(myfunction(), silent = TRUE)
  
  if (inherits(result, "try-error")) {
    return("laatste model")
  }
  
  "volgende model"
}


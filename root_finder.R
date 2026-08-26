find_roots <- function(formula, intervalStart, intervalEnd) {
  # first check if the intervalsthere is a root between the intervals
  start_value <- eval(expr, envir = list(x = intervalStart))
  end_value <- eval(expr, envir = list(x = intervalEnd))
  
  product <- start_value * end_value
  if (product > 0) {
    stop("No possible root in the given interval.")
  }
  
  if (start_value == 0) {
    return(intervalStart)
  }
  
  if (end_value == 0) {
    return(intervalEnd)
  }
  
  
  
}
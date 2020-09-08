# Lucas Number Recursion in R
lucas <- function(n) {
  if (n == 0){
    return(2)
  }
  else if (n == 1){
    return(1)
  }
  else {
    return(lucas(n-1)+lucas(n-2))
  }
}

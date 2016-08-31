addconv <- function(x) {
  
  y <- if(x > 0) {
    1
  } else {
    -1
  }
  
  x+y
}

subsi <- function(x) {
  x[x>10]
}

subsi_n <- function(x, n = 10) {
  x[x>n]
}

col_mean <- function(x, removeNA = TRUE) {
  nc <- ncol(x)
  vmeans <- numeric(nc)
  
  for(i in 1:nc) {
    vmeans[i] <- mean(x[,i], na.rm = removeNA )
  }
  
  vmeans
}
# Creates a multiplication matrix of size num
create.ttable <- function (num) {
  x <- 1:num
  y <- 1:num
  #allocate a matrix of num x num dimensions
  z <- matrix(1:(num*num), nrow=num, ncol=num)
  
  for (i in 1:length(x)) {
    for (j in 1:length(y)) {
      z[i,j] <- x[i] * y[j]  
    }
    
  }
  return (z) # return matrix of values
}

# test the function to create a matrix of the 9 times table
number.in <- as.integer(9)
ttable <- create.ttable(number.in)
cat(sprintf ("The %d times table\n", number.in))

for (i in 1:number.in) {
  for (j in 1:number.in) {
    val <- sprintf("%3d   ",ttable[i,j])
    cat(val)
  }
  cat("\n")
}

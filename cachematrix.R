## (sorry for my english...) Those two functions take the elements of a square matrix as a vector of r*c integer (r = c): 
## the first, makeCacheMatrix(), create a "special" matrix, the second one inverts the matrix a stores it in memory.



## This function creates a matrix by taking vector of r*c numeric (the matrix has to be square), so the dimensions 
## of the matrix are the squareroot of the length of the vector.
## At the beginning of the function, the variable "inversa" is set NULL, the function "set()" create the matrix 
## (ncol and nrow are equal), and the the function "get()" print the matrix.
makeCacheMatrix  <- function(x = numeric(), r, c) {
  inversa <- NULL
  set <- function(y = numeric(), r, c) {
    x <<- matrix(y, r, c)
    inversa <<- NULL
    }
  get <- function() matrix(x, r, c)
  set.inv <- function(solve) inversa <<- solve
  get.inv <- function() inversa
  list(set = set, get = get, set.inv = set.inv, get.inv = get.inv)
}



## The function cacheSolve() verify if the inverse matrix is allocate yet: if no (is.null(inversa)) the 
## function "set.inv <- function(solve) inversa <<- solve"  calculates it, if yes (!is.null(inversa)), prints it by.

cacheSolve <- function(x) {
  inversa <- x$get.inv()
  if(!is.null(inversa)) {
    message("getting cached data")
    return(inversa)
  }
  data <- x$get()
  inversa <- solve(data)
  x$set.inv(inversa)
  inversa
}


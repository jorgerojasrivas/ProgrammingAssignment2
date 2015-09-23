## makeCacheMatrix is a function that creates a "matrix" object that caches 
## its inverse. 
## cacheSolve does the actual computation of the inverse of the object returned
## by makeCacheMatrix. cacheSolve uses the solve(X)function in R. 

## If the inverse has already been calculated, and the matrix has not changed,
## then cacheSolve gets the inverse from cache memory. 

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y){
    
    x <<- y
    m <<- NULL
    
  }
  
  get <- function() x
  set_inverse <- function(inverse) m <<- inverse ## scoping rules in R
  get_inverse <- function () m
  
  list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)

}


## cacheSolve checks to see if there is an inverse matrix already calculated.
## If so, gets the inverse from cache memory and avoids a time-consuming calculation

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$get_inverse()
  ## checks to see if the inverse has been already calculated
  if(!is.null(m)){
    
    message("getting inverse from cache data")
    return(m)
    
  }
  ## gets the value of the matrix whose inverse is needed
  data <- x$get()
  
  ## uses solve() to calculate the inverse
  m <- solve(data)
  
  ## delivers the inverse
  
  x$set_inverse(m)
  m
  
  ## to test the function type in cacheSolve(makeCacheMatrix(matrix(c(1,2,3,4), nrow = 2, ncol = 2)))
}

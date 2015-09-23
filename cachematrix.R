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
  set_inverse <- function(iinverse) m <<- inverse
  get_inverse <- function () m
  
  list(set = set, get = get, set_inverse )

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}

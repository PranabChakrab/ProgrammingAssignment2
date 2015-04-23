## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    ## This function, makeCacheMatrix creates a special "vector", 
    ## which is really a list containing a function to
    ## 1. set the value of the vector
    ## 2. get the value of the vector
    ## 3. set the value of the matrix inverse
    ## 4. get the value of the matrix inverse
    
    x_inverse <- NULL
    
    ## The set function
    set <- function(y) {
      x <<- y
      x_inverse <<- NULL
    }
    
    ## The get function
    get <- function() x
    
    ## The setinverse function
    setinverse <- function(inverse) x_inverse <<- inverse
    
    ## The getinverse function
    getinverse <- function() x_inverse
    
    ## Create the list
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
  
    x_inverse <- x$getinverse()
    
    ## Check if it is already in the cache then return that value
    if(!is.null(x_inverse)) {
      message("getting cached data")
      return(x_inverse)
    }
    
    ## If not in cache, get the data first
    data <- x$get()
    
    ## Generate the matrix inverse (assumption: the matrix is invertible)
    x_inverse <- solve(data, ...)
    
    ## Set the inverse data and then return it
    x$setinverse(x_inverse)
    x_inverse
}

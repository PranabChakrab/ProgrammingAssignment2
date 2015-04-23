## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly.
## The following pair of functions cache the inverse of a matrix 
## functions do

## makeCacheMatrix: This function creates a special "matrix" object that can 
## cache its inverse. It creates a special "vector", which is really a list 
## containing a function to
## 1. set the value of the vector
## 2. get the value of the vector
## 3. set the value of the matrix inverse
## 4. get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
    
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
    
    ## Creates the list
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above.If the inverse has already been calculated 
## (and the matrix has not changed),  then the cachesolve should retrieve the 
## inverse from the cache. 

cacheSolve <- function(x, ...) {
    ## Returns a matrix that is the inverse of 'x'
  
    x_inverse <- x$getinverse()
    
    ## Checks if it is already in the cache then return that value
    if(!is.null(x_inverse)) {
      message("getting cached data")
      return(x_inverse)
    }
    
    ## If not in cache, get the data first
    data <- x$get()
    
    ## Generates the matrix inverse (assumption: the matrix is invertible)
    x_inverse <- solve(data, ...)
    
    ## Set the inverse data and then return it
    x$setinverse(x_inverse)
    x_inverse
}

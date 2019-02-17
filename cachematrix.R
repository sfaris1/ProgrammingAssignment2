## makeCacheMatrix is a function whose purpose is to store the inverse value of a matrix which can then be retrieved
## by the cacheSolve function without having to run extensive inverse matrix calculations every time. 

## makeCacheMatrix outputs a list of functions that can be called upon by its partner function, cacheSolve (below).
## This function takes a matrix as an argument and makes use of the accessor and mutator methods in R (setters and getters)
## First, it assigns a null value to a variable "i", which will be used to store the value of the inverse of the matrix
## Using nested functions it sets the value of a matrix, gets the value of the matrix, sets the value of the inverse of the matrix and gets the value of the latter

makeCacheMatrix <- function(x = matrix()) {
                    i <- NULL
                    
                    set <- function(y) {
                         x <<- y
                         i <<- NULL
                     }
  
                      get <- function() x
                     setinverse <- function(solve) i <<- solve
                     getinverse <- function() i
      list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## cacheSolve works in tandem with makeCacheMatrix. Its purpose is to return the inverse of a matrix previously used in calling
## the makeCacheMatrix function. First, cacheSolve tries to find whether there is already a stored value for the inverse "i" by invoking the
## getinverse function. If it finds it, it displays "i". If not, it gets the value of our matrix using the get function, 
## calculates its inverse and then invokes setinverse to store the inverse value. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
         message("getting cached data")
        return(i)
         }
  
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}

## Caching the Inverse of a Matrix:
## Objective: Cache the inverse of a matrix to avoid the costly commputation
##    of repeatedly computing the inverse of an invertible matrix
##
## Below are a pair of functions that are used to create a special object that
##  stores a matrix and caches its inverse. 
##
## The following function creates a special "matrix" object that can cache its inverse.
## It is assumed that the matrix supplied is always nonsingular e.g. invertible.  

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {                            # function to set value of matrix
    x <<- y
    inv <<- NULL
  }
  get<-function() x                               # function to get or retrieve value of matrix
  setinverse<-function(inverse) inv<<-inverse     # function to set value of the inverse
  getinverse<-function() inv                      # function to get or retrieve value of inverse
  list(set = set,                                 # returns a list of the above functions
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" using the
##    makeCacheMatrix above. If the inverse has already been calculated (and
##    the matrix has not changed), then it retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()                           # retrieves the inverse
  if (!is.null(inv)) {                            # if inverse exists, retrieves the inverse from
    message("getting cached data")                #     the cache and returns it
    return(inv)
  }
  thematrix <- x$get()                            # otherwise, retrieves the matrix
  inv <- solve(thematrix, ...)                    # solves for the inverse
  x$setinverse(inv)                               # stores the inverse in the cache for future reference
  inv                                             # returns the inverse
}

## In this assignment, my job is to write a pair of functions, "makeCacheMatrix" and "cacheSolve," 
## that cache the inverse of a matrix.

## "makeCacheMatrix" creates a special matrix object that is able to cache its inverse. 

makeCacheMatrix <- function(x = matrix()) 
{
        inv = NULL
        set = function(y) 
           {
             x <<- y
             inv <<- NULL
            }
        get = function() x
        setinv = function(inverse) inv <<- inverse 
        getinv = function() inv
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}

## "cacheSolve" computes the inverse of the matrix object returned by makeCacheMatrix(). 
## If the inverse has already been calculated and the matrix has not changed, then "cacheSolve" will retrieve
## the inverse from the cache directly.

cacheSolve <- function(x, ...) 
{
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) 
        {
          message("getting cached data")
          return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}

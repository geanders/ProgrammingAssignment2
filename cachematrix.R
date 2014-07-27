## Create a special object that will determine and store the 
## inverse of a matrix

## Create a list object that includes a function to set the values 
## of the matrix and its inverse as well as get the values of the 
## matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inv <<- solve
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Solve for inverse of the matrix. Check first to see if the inverse has
## already been solved and added to the cache, in which case pull the
## inverse from cache rather than re-calculating.

cacheSolve <- function(x, ...) {
       m <- x$getinverse()
       if(!is.null(m)){
               message("getting cached data")
               return(m)
       }
       mat <- x$get()
       inv <- solve(mat)
       x$setinverse(inv)
       inv
}

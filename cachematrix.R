## Calculating the inverse of large matrices could be an expensive proposition
## the followingtwo functions allow the caching of such an operation allowing
## faster access to inverse matrix operations on the same matrix.

## makeCacheMatrix 
## 
##creates a list containing a function to
##
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse of the matrix
## get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse<- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve returns the inverse of the matrix. 
## It first checks if the inverse has already been calculated and cached
## If a cached copy is found, return cache and avoid an unnecessary recalculation
## If a cached copy is not found, it will calculate the inverse and cache the result.

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

## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## this function receives a matrix rather than a numeric value for its argument.
## let m be the inversed matrix
## let x be the original matrix that we need to solve
## the get and set functions help us retrieve the unsolved matrix (x) or assigning it respectively.
## setinverse and getinverse help us retrieve the inverse matrix (y)
makeCacheMatrix <- function(x = matrix()) {
        m   <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(matrixVariable) m <<- matrixVariable
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}

## Write a short comment describing this function
## this function tries to get the inverse matrix from the object x
## the if statement checks if the inverse matrix of x is available.
## if it does it returns the inverse matrix using cache.
## if not, we retrieve the unsolved matrix
## solves x
## and set the inverse matrix back into x
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse() ## tries to get the inverse matrix
        if(!is.null(m)) { ## if the inverse matrix is available. 
                message("getting cached data")
                return(m) ##returns the inverse matrix 
        }
        data <- x$get() ## if not we get the unsolved matrix
        m <- solve(data, ...) ## solves it
        x$setinverse(m) ## and set it back into x
        m
}

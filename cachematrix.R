#makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(m = matrix()) {
        i <- NULL #initialize vector to hold the inverse
        set <- function(y) { #set the matrix 'y'
                m <<- y
                i <<- NULL
        }
        
        get <- function() m #get the matrix 'm' and return it
        setinv <- function(invert) i <<- invert #set the inverse of the matrix
        getinv <- function() i # get the inverse of the matrix
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv) # return a list of the above methods
}

#This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
cacheSolve <- function(x, ...) {
        m <- x$getinv() # get the inverse of the matrix
        if(!is.null(m)) { # return the inverse if it's already cached
                message("getting cached data")
                return(m) # return the matrix
        }
        
        data <- x$get() # get the matrix data
        m <- solve(data) %*% data #calculate the inverse of the matrix held in 'data'
        x$setinv(m) # set the inverse
        m # return the matrix
}

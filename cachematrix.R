## These functions cache the inverse of a matrix.

## This function makes the matrix that will set and get the value of the matrix, 
##and set and get the value of the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setmatrix <- function(solve) i <<- solve
        getmatrix <- function() i
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}



##This function calculates the inverse of the matrix, reusing the cached result if available. 

cacheSolve <- function(x=matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getmatrix()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        matrix <- x$get()
        i <- solve(matrix, ...)
        x$setmatrix(i)
        i
}


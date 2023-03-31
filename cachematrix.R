## Returns a "special" matrix from the matrix you pass to it which
## has ability to cache its inverse. This function defines functions 
## on the matrix which set new value of the matrix and to set and 
## get inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse
        )
}


## Takes as argument the "special" matrix defined by makeCacheMatrix
## function, and provides inverse of the matrix, if a cached copy
## of the inverse is available, that is returned, otherwise inverse
## is created, cached and returned. Inversing a matrix is a 
## costly operation so this function should be used.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if (!is.null(inv)) {
                message("getting cached inverse of the matrix")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}

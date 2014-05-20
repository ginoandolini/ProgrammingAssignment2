## makeCacheMatrix makes a special matrix
## Sets the value of the matrix, set
## Gets the value of the matrix , get
## Sets the value of the matrix inverse, setinverse
## Gets the value of the matrix inverse, getinverse

makeCacheMatrix <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## Calculates the inverse of the special "matrix" created with makeCacheMatrix
## First checks to see if the inverse has already been calculated
## If so gets the inverse from the cache and skips the computation
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setinverse function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
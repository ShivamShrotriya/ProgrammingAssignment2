## Catching inverse of a matrix

# makeCacheMatrix command creates a object (as list) that holds original Matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
              set <- function(y) {
                    x <<- y
                    m <<- NULL
              }
              get <- function() x
              setmatrix <- function(solve) m <<- solve
              getmatrix <- function() m
              list(set = set, get = get,
                   setmatrix = setmatrix,
                   getmatrix = getmatrix)
}

#cacheSolve command returns a matrix that is the inverse of original Matrix. If the inverse is already calculated,
# it takes the inverse from the catch, otherwise calculates the inverse
cacheSolve <- function(x, ...) {
        m <- x$getmatrix()
        if(!is.null(m)) {
            message("getting cached inverse matrix")
            return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setmatrix(m)
        m
}

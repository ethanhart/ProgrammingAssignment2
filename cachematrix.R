## makeCacheMatrix will solve a matrix and cache the result

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInvMatrix <- function(inverse) m <<- inverse
    getInvMatrix <- function() m
    list(set = set, get = get,
         setInvMatrix = setInvMatrix,
         getInvMatrix = getInvMatrix)

}


## cacheSolve will solve a matrix by first attempting to retrieve the result from a cache, otherwise solving normally

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInvMatrix()
        if(!is.null(m)) {
            message("getting cached inverse matrix")
            return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setInvMatrix(m)
        m
}

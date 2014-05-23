## makeCacheMatrix will solve a matrix and cache the result
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL # This is the cached result
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    
    # Set the cached value (m) to be the inverse matrix
    setInvMatrix <- function(inverse) m <<- inverse
    
    # Return m (the cached result)
    getInvMatrix <- function() m
    
    # Create a list so functions can be easily referenced
    list(set = set, get = get,
         setInvMatrix = setInvMatrix,
         getInvMatrix = getInvMatrix)

}


## cacheSolve will solve a matrix first by attempting to retrieve the result from a cache,
## otherwise solving the inverse using solve()
cacheSolve <- function(x, ...) {
    # Gets cached data- may or may not be a matrix
    m <- x$getInvMatrix() 
        
        
    # Check if cache exists
    # If so, return cached inverse matrix
    if(!is.null(m)) {
        message("getting cached inverse matrix")
        return(m)
    }
    
    # If cache doesn't exist, get original matrix
    # Solve inverse with solve() function
    # Set chache with solved inverse matrix
    data <- x$get()
    m <- solve(data)
    x$setInvMatrix(m)
    m
}
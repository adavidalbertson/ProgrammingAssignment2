## create a matrix object that caches its inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    get <- function() { x }
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    getInverse <- function() { inv }
    setInverse <- function(inverse) { inv <<- inverse }
    
    list(
        get = get,
        set = set,
        getInverse = getInverse,
        setInverse = setInverse
    )
}


## find the inverse of a makeCacheMatrix objectc
cacheSolve <- function(x, ...) {
    # try and get the cached inverse
    inv <- x$getInverse()
    
    # if the inverse isn't cached, compute it
    if(is.null(inv)) {
        inv <- solve(x$get(), ...)
        x$setInverse(inv)
    }
    
    #return
    inv
}

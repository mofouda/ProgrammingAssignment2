## makeCacheMatrix
 # This function creates a special "matrix", which is really a list
 # containing a function to:
        # 1 set the value of the matrix
        # 2 get the value of the matrix
        # 3 set the value of the inverse of the matrix using the solve() function
        # 4 get the value of the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
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


## cacheSolve
 # This function computes the inverse of the special "matrix" returned by 
 # makeCacheMatrix above. If the inverse has already been calculated (and the 
 # matrix has not changed), then the cachesolve should retrieve the inverse 
 # from the cache and skips computation. Otherwise, it calculates the inverse of 
 # the data and sets the value of the inverse in the cache via the Solve() function.


cacheSolve <- function(x, ...) {
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

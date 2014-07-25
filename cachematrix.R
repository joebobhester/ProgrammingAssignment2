## The makeCacheMatix function creates a vector containing a function
## 1. to set the value of the vector,
## 2. to get the value of the vector,
## 3. to set the value of the matrix inverse,
## 4. to get the value of the matrix inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## The cacheSolve function calculates the inverse of the matrix
## from the makeCacheMatrix function above. 
## First, it checks to see if the inverse has already been calculated.
## If so, it gets the inverse from the cache. Otherwise, it 
## calculates the inverse and sets the value in the cache.

cacheSolve <- function(x, ...) {
         m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}


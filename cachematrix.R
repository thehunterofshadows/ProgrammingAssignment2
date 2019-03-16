## These functions will create a specal matrix that can 
## cache it's inverse

## Function that creates a speical "matrix" that can store/cache it's inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL  #this is how you know if the matrix changed.  if the set fucntion is called, it will set m to NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Determine if the inverse has already been calculate, if so return it, if not calculate it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setinverse(m)
    m
}


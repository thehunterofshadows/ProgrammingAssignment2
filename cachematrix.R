## These functions will create a specal matrix that can 
## cache it's inverse

## Function that creates a speical "matrix" that can store/cache it's inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    ##create set function to set the matrix
    set <- function(y) {
        x <<- y
        m <<- NULL  #this is how you know if the matrix changed.  if the set fucntion is called, it will set m to NULL
    }
    ##create the get function to return the matrix
    get <- function() x
    ##create the setinverse function to store the value
    setinverse <- function(inverse) m <<- inverse
    ##create the getinverse function to return the cached value
    getinverse <- function() m
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Determine if the inverse has already been calculate, if so return it, if not calculate it

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    #calulate the inverse if it does not exist
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    #if not, calculate the inverse cache, and return
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}


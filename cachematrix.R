## Notes
## makeCacheMatrix - Change class eaxample from list to matrix, and we will do inverse instead of mean
## cacheSolve - compute inverse of matrix.  
## if already calculated AND matrix not changed, then retrieve inverse from cache
## how do you know if the matrix has changed?
## how to inverse - solve(x) if matrix is square and invertable


## assume the matrix is always invertable

## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL  #this is how you know if the matrix changed.  if the set fucntion is called, it will set m to NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    ##I understand we want a list here, not a matrix, because the list
    ##will hold the functions we are giving it.  It will store the matrix
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function

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

##test scenarios
## test nonsquare

##done
##create, set matrix with new matrix, set inverse, change data and set inverse,
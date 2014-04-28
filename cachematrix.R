## This two functions are used to compute the inverse of
## invertible square matrices. However, because matrix
## inversion is a costly computation, the result of the
## last computation is stored in a variable "i". Therefore,
## if it is desirable to get the inverse of the matrix
## again, it is retrieved from the variable, skipping the
## repeated computation of the marix inversion.

## This function receives a matrix and creates a 4-element
## list. Each element is a function. "set" function
## receives a new matrix and substitutes it for the
## previous matrix in cache. It also sets the inverse
## matrix associated with the previous matrix to "NULL".
## The output of "get" function is the current matrix in
## cache; it is used in the second part of this script for
## the calculation of the inverse matrix. "setinverse"
## recieves the inverse matrix calculated in the second
## part of the script and stores it in the variable "i",
## so the result of calculating the inversion is ready in
## cache for later probale calls. "getinverse" gives back
## the value stored in "i"; therefore, when "getinverse"
## is called, if gives back the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) i <<- inv
    getinverse <- function() i
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This second part of the script is where the calculation
## of the inverse matrix occurs. It receives the list
## created by "makeCacheMatrix" function in the first part.
## If the calculation has been done once before, the
## inverse matrix is stored in the "i" variable in the
## environment of the "makeCacheMatrix" function. Therefore,
## calling "getinverse" retrieves the inverse matrix from
## cache, obviating a need to recalculate the inverse.
## The value of the inverse matrix is returned, with the
## message "getting cached data" and the execution of
## "cacheSolve" ends. If inversion is done on a new matrix,
## the value of "i" in the environment of "makeCacheMatrix"
## is equal to "NULL" and, consequently, what "getinverse"
## brings back is "NULL". Now the calculation of the
## inverse is done as follows. "data" is assigned the value
## of the matrix whose inverse is to be calculated, i.e.
## the output value of "x$get()". The function "solve"
## calculates the inverse of the matrix. The value of the
## inverse matrix is fed into the "x$setinverse" function
## so "x$setinverse" causes the inverse matrix to be stored
## in the cache as the variable "i" in the environment of
## "makeCacheMatrix". Finally, the calculated inverse
## is returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if (!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}

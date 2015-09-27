## A pair of functions caching the Inverse of a Matrix

## Function makeCacheMatrix creates a matrix object, that can cache its inverse
###Input to this function should be an invertible square matrix on numbers

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL                             #creates an empty matrix object, which will be used later to store the inverse matrix
        set <- function(y) {                    #function which rewrites the x matrix by a new input matrix (if the makeCacheMatrix is called for a new input matrix)
                x <<- y
        }
        get <- function() x                     #function which returns the current input matrix
        setinverse <- function(solve) inv <<- solve #function which rewrites the inverse matrix by the result of cacheSolve function
        getinverse <- function() inv            #function which returns the current inverse matrix
        list(set = set, get = get,              #stores all of the 4 functions in the function makeCacheMatrix
             setinverse = setinverse,
             getinverse = getinverse)
}

## Function cacheSolve 'calculates & prints' or 'calls from the cache & prints' the inverse of the matrix x

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()                   #assigns to inv object inverse matrix (if there is already any stored in cache)
        if(!is.null(inv)) {                     #verifies the object inv, stored previously with getinverse, if it exists and is not NULL
                message("getting cached data")  #message which is returned, if the inverse matrix for current data was already calculated (is not empty)
                return(inv)                     #prints the inverse matrix from the cache
        }
        data <- x$get()                         #if the inverse matrix was not calculated before,this part of the code gets the current input matrix,
        inv <- solve(data, ...)                 ##calculates its inverse matrix,
        x$setinverse(inv)                       ##assignes it into the cache
        inv                                     ##and prints the result without message "getting cached data"
}

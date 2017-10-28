## The 'makeCacheMatrix' function creates a special "matrix" that can cache its inverse.
## This function contains functions to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
                
         }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
        }
        

## Function "cacheSolve" can be used to get the inverse of a square matrix quickly.
## If the inverse has already been calculated, then the function should retrieve the inverse from the cache.
## This function assumes that the matrix is always invertive.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
        }

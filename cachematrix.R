## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly. 
## Below a pair of functions that cache the inverse of a matrix.

## makeCacheMatrix: This function creates a special "matrix" object 
## that can cache its inverse as following:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinverse(m)
        m               
}


## Test run
#> source("makeCacheMatrix.R")
#>  
#> x <- makeCacheMatrix()
#> 
#> x$set(matrix(c(-1,-2,1,1),2,2))
#> 
#> x$get()
#[,1] [,2]
#[1,]   -1    1
#[2,]   -2    1
#> 
#> cacheSolve(x)
#[,1] [,2]
#[1,]    1   -1
#[2,]    2   -1
#> 
#> cacheSolve(x)
#getting cached data
#[,1] [,2]
#[1,]    1   -1
#[2,]    2   -1
#> 
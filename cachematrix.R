## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## Put comments here that give an overall description of what your functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL                     ## set inv as NULL for inverse Matrix           
        set <- function(y) {            ## define "set" func.
                x <<- y
                inv <<- NULL
        }
        get <- function() x             ## define "get" func., get matrix value
        setInverse <- function(Inverse) inv <<- Inverse
                                        ## set inverse matrix value
        getInverse <- function() inv    ## get inverse matrix value
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}
## cacheSolve: This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix has
## not changed), then the cachesolve should retrieve the inverse from the cache.

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if(!is.null(inv)) {             ## inv exsited, get inverse matrix from cached
                message("getting cached data")
                                        ## print out "getting cached data"
                return(inv)
        }
        data <- x$get()                 ## if inv isn't exsited, get matrix value
        inv <- solve(data, ...)         ## get inverse Matrix by solve()
        x$setInverse(inv)               
        inv                             ## return a matrix that is the inverse of 'x'
}

## test function

## my_mat<-makeCacheMatrix(matrix(3:6, 2, 2))
## cacheSolve(my_mat)
##      [,1] [,2]
## [1,]   -3  2.5
## [2,]    2 -1.5
## > my_mat$get()
##      [,1] [,2]
## [1,]    3    5
## [2,]    4    6
## > cacheSolve(my_mat)
## getting cached data
##      [,1] [,2]
## [1,]   -3  2.5
## [2,]    2 -1.5
## > my_mat$getInverse()
##      [,1] [,2]
## [1,]   -3  2.5
## [2,]    2 -1.5

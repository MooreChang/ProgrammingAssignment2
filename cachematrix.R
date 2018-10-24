## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL             ## set inv as NULL for inverse Matrix
        set <- function(y) {    ## define "set" func.
                x <<- y
                inv <<-NULL
        }
        get <- function() x     ## define "get" func.
        setInverse <- function(Inverse) inv <<-Inverse
        getInverse <- function() inv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        inv <-x$getInverse()
        if(!is.null(inv)){      ## if inv not empty, return cached inv
                message("getting cached data")
                return(inv)
        }
        data <- x$get()         ## 
        inv <-solve(data, ...)  ## get inverse Matrix by solve()
        x$setInverse(inv)
        ## Return a matrix that is the inverse of 'x'
        inv
}

## test function
## my_mat<-makeCacheMatrix(matrix(3:6, 2, 2))
## cachesolve(my_mat)

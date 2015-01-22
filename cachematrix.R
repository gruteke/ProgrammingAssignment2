## Put comments here that give an overall description of what your
## functions do
## 
## These functions give possibility to get cached result of inversed matrix
## In case inversed matrix does not exist it will calculate the inversed matrix
## In case inversed matrix already exists it will be recalled from the cache
## mdat <- matrix(c(1,2, 11,12), nrow = 2, ncol = 2, byrow = TRUE, dimnames = list(c("row1", "row2"), c("C.1", "C.2")))
## print(mdat)
## cachemat <- makeCacheMatrix(mdat)
## cacheSolve(cachemat)
## cacheSolve(cachemat)
## In first call to cacheSolve the matrix is calculated, in second call it will be retrieved from cache


## Write a short comment describing this function
## This function is a function that creates a list of helper functions that sets the matrix and nullifies the cached inversed matrix (set), 
## retrieves the matrix (get), sets the inversed matrix in cache (setinv) and gets the inversed matrix from cache when exists (getinv)
## Input is the matrix, the list of helper functions will be returned.

makeCacheMatrix <- function(x = matrix()) {
		xinv <- NULL
        set <- function(y) {
                x <<- y
                xinv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) xinv <<- solve
        getinv <- function() xinv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Write a short comment describing this function
## This function uses the helper function as input
## First it tries to get the inversed matrix from cache. If result is not null it will print message "getting cached data" and return with cached inversed matrix
## In case inversed cached matrix does not exist the matrix will be read using the get helper function. Then the inverse matrix will be calculated using the solve function
## The inversed matrix will be put in cache using the setinv helper function and the inversed matrix will be returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		xinv <- x$getinv()
        if(!is.null(xinv)) {
                message("getting cached data")
                return(xinv)
        }
        data <- x$get()
        xinv <- solve(data, ...)
        x$setinv(xinv)
        xinv
}



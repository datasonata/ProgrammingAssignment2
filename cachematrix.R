## The first function creates and caches the inversed matrix in variable ix
## The second function will use the product of the first function to either computer solve or use cache inversed matrix

## HOW TO RUN THIS SCRIPT
## 1) source("cachematrix.R")
## 2) create a matrix and save is in variable m. 
##	For example, m=rbind(c(1, -1/8), c(-1/8, 1))
## 3) execute the following:
##	f <- makeCacheMatrix(m)
## 4) execute the cacheSolve function as follows:
##	cacheSolve(f)

## the first run will force the function to compute the solve() 
## the consecutive runs will use the cached data with message displayed "getting cached inversed matrix"

makeCacheMatrix <- function(x = matrix()) {
	## ix is the variable for reversed x variable if x is invertible matrix
        ix <- NULL
        set <- function(y) {
                x <<- y
                ix <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) ix <<- inverse
        getinverse <- function() ix 
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


# the following function will check if inverse matrix already exists first,
# if not, it will create it using the solve() function 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ix <- x$getinverse()
        if(!is.null(ix)) {
                message("getting cached inversed matrix")
                return(ix)
        }
        data <- x$get()
        ix <- solve(data, ...)
        x$setinverse(ix)
        ix
}

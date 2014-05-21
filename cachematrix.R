## The first function creates and caches the inversed matrix in variable ix
## The second function will use the product of the first function to either computer solve or use cache inversed matrix

## if m is your matrix, for example, m=rbind(c(1, -1/8), c(-1/8, 1)), then
## 1) call f <- makeCacheMatrix(m)
## 2) then call cacheSolve(f)

## the first run will force the function to compute the solve() 
## the consequtive runs will use the cached data with message displayed "getting cached inversed matrix"

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


## Write a short comment describing this function

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

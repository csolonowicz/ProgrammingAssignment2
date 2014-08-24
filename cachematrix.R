## The first function, makeCacheMatrix creates a special "vector", which is really a list containing a function to 
## 1.set the value of the matrix
## 2.get the value of the matrix
## 3.set the value of the inverse matrix
## 4.get the value of the inverse matrix


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(matrix) m <<- matrix
        getmatrix <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}


## The following function calculates the inverse matrix
## or gets already calculated value which is cached 
## and returns the value to calling environment

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        m <- x$getmatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
	  message("getting none cached data")
        x$setmatrix(m)
        m

}

## Unit Testing Instructions
## Create sample inversible matrix
##    	[,1] [,2]
##	[1,]    1    1
##	[2,]   -1    2

## > x<-matrix(c(1,-1,1,2),nrow=2,ncol=2)
## > makeCacheMatrixPointer <- makeCacheMatrix(x)   ## local scope
## > cacheSolve(makeCacheMatrixPointer)
## getting none cached data
##          [,1]       [,2]
## [1,] 0.6666667 -0.3333333
## [2,] 0.3333333  0.3333333
## > cacheSolve(makeCacheMatrixPointer)
## getting cached data
##           [,1]       [,2]
## [1,] 0.6666667 -0.3333333
## [2,] 0.3333333  0.3333333




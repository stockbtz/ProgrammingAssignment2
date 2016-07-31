## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
                i <- NULL
                set <- function(y) {
                        x <<- y
                        i <<- NULL
                }
                get <- function() x
                setInverse <- function(Inverse) i <<- Inverse
                getInverse <- function() i
                list(set = set, get = get,
                     setInverse = setInverse,
                     getInverse = getInverse)
        
        
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Check whether matrix is invertible or not
        if(dim(x)[1] == dim(x)[2]){
                ## Return a matrix that is the inverse of 'x'
                i <- x$getInverse()
                if(!is.null(i)) {
                        message("getting cached data")
                        return(i)
                }
                data <- x$get()
                i <- solve(data)
                x$setInverse(i)
                i
        } else print("Your matrix is not invertible") ##warning message and do nothing
}

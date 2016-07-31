## Both of this functions main purpose is to help calculate Inverse of matrix
## By saving already-calculated-inverse of matrix in a cache, it helps optimizing calculation time

## this function creates special type of matrix for calculation where 1-4th objects of output
## return functions

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


## This function use special matrix(a product from above function)
## and calculate their inverse
## if the inverse of this matrix was calculated it returns a cached inverse
## else new inverse will be calculated

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


## The first function, makeCacheMatrix creates a "vector", which is containing a function 
## to set and get values of the vector and its inverse

makeCacheMatrix <- function(x = matrix()) {
        k <- NULL
        set <- function(y){
                x <<- y
                k <<- NULL
        }
        get <- function()x
        setInv <- function(inverse) k <<- inverse
        getInv <- function() k
        list(set=set, get=get,
             setInv = setInv,
             getInv = getInv)
}
        
## The following function, gets the inverse of the matrix created with the above function,
## cheching if this inverse has already been calculated.

cacheSolve <- function(x, ...) {
        k <- x$getInv()
        if(!is.null(k)) {
                message("getting cached data")
                return(k)
        }
        dataInv <- x$get()
        k <- solve(dataInv, ...)
        x$setInv(k)
        k
}

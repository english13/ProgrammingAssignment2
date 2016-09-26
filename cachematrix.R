## Instead of repeatedly calculating the inverse of a matrix, we check the cache for whether it's already been calculated. To do 
## this, we equate the original matrix to a variable and immediately check whether that variable's been "done" in the cache. 

## The "set" function enables the program to pull from the global environment. "get" is just the original matrix. "setinv" changes
## the null matrix to the inverse if given the inverse. "getinv" is the inverse. 

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<-y
                inv <<-NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get=get, 
             setinv = setinv,
             getinv = getinv)
        

}


##checks if the inverse is null and if not, it was pulled from the cache. If it is, then the inverse is calculated. 

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv = Solve(data, ...)
        x$setinv(inv)
        inv
}

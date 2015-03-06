## Put comments here that give an overall description of what your
## Computing the inverse of a matrix is quite costly. In the operation needs to be computed multiple times
## then these two functions will help save this computation in cached memory. 

## Write a short comment describing this function
## First, create the matrix that has the caching cability by providing the 
## makeCacheMatrix function with the matrix to be cached.
makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
    set <- function(y) {
            x <<- y
            inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Write a short comment describing this function
## In order to call the inverse of the cahced matrix, simply call the cacheSolve function with the cached 
## matrix vairable.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()

        ## Check to see if the inverse is already in cache, if so then retrive it from memory.
        if(!is.null(inv)) {
                #message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}

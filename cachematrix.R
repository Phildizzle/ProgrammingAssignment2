## Put comments here that give an overall description of what your
## functions do

## The first "makeCacheMatrix" function creates a matrix which can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
} 

## The second "cacheSolve"-matrix creates the inverse computed by the 
## first "maakeCacheMatrix" function. If the inverse has already been calculated        
## then it retrieves the inverse from its cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting the cached data...")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}

## Testing the matrix code
my_mat <- makeCacheMatrix(matrix(1:4, 2, 2))
my_mat$get()
my_mat$getInverse()
cacheSolve(my_mat)

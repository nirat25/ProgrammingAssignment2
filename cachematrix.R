## makeCacheMatrix can either get or set a supplied matrix to cache memory as well as set or get  the inverse of the matrix from cache memory 
## cacheSolve checks the cache memory to see if the inverse matrix has already been calculated and if so it gives output from cache or else calculates the inverse matrix

## makeCacheMatrix will return a list of functions to either get or set the supplied matrix or the inverse of the matrix 

makeCacheMatrix <- function(x = matrix()) {

        inv <- NULL
        set <- function(y) {
                
                x <<- y
                inv <<- NULL
        }
        
        get <- function() x
        
        setinverse <- function(inverse) inv <<- inverse
        
        getinverse <- function() inv
        
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
        
}


## cacheSolve will check the cache to see if the computed inverse of the matrix already eists or not and if not wil compute the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        mat <- x$getinverse()
        if(!is.null(mat)){
                message("getting cached data")
                return(mat)
        }
        
        comp <- x$get()
        mat <- solve(comp, ...)
        x$setinverse(mat)
        mat
        
}


## Creates a matrix object, containing the list of the functions to help cache the matrix inverse computation.
makeCacheMatrix <- function(x = matrix()) {
        mat <- NULL
        set <- function(y) {
                x <<- y
                mat <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) mat <<- solve
        getinverse <- function() mat
        list(set = set, get = get, setinverse = setinverse, 
             getinverse = getinverse)
        
}


#Saves computation by checking if the inverse is already calculated and uses that value, if not, then it calculates it.

cacheSolve <- function(x, ...) {
        mat <- x$getinverse()
        if(!is.null(mat)) {
                message("getting cached data")
                return(mat)
        }
        data <- x$get()
        mat <- solve(data, ...)
        x$setinverse(mat)
        mat
}
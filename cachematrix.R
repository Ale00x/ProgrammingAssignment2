## Programming Assingment 2 - Coursera
## These functions will cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(m = matrix()) {
        
        ## the inverse property
        i <- NULL
        
        ## set the matrix
        set <- function (matrix) {
                m <<- matrix
                i <<- NULL
        }
        
        ## get the matrix
        get <- function() {
               ## return the matrix
                m
        }
        
        ## set the inverse of the matrix
        setInverse <- function (inverse) {
                i <<- inverse
        }
        
        ## get the inverse 
        getInverse <- function(inverse) {
                ## return the inverse matrix
                i
        }
        
        ## return a list of the functions
        list(set = set, get = get)
             setInverse = setInverse
             getInverse = getInverse   
}

## Compute the inverse of the matrix returned by "makeCacheMatrix" above. 
## If the inverse has already been calculated (and the matrix has not
## changed), then the "cachesolve" should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
       
        ## return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        
        ## return the inverse if its already set
        if(!is.null(m)) {
                message ("getting cached data")
                return(m)
        }
        
        ## get the matrix from object
        data <- x$get()
        
        ## calculate the inverse using matrix multiplication
        m <- solve(data) %*% data
        
        ## set the inverse to the object
        x$setInverse(m)
        
        ## return the matrix
        m
}

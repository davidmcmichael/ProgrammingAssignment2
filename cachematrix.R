## two functions that cache the inverse of a matrix

## Creates a matrix that can stores in memory its own inverse

makeCacheMatrix <- function(m = matrix() ) {

        ## Initialize parameter to store inverse
        y <- NULL
        
        ## store the matrix
        store <- function(matrix) {
          m <<- matrix
          y <<- NULL
        }
        
        ## retreive the matrix
        getMatrix <- function() {
          m
        }
        
        ## set the inverse of the matrix
        storeInverse <- function(inverse) {
          y <<- inverse
        }
        
        ## retrieve the inverse of the matrix
        getInverse <- function () {
          y
        }
}


## Calculate the inverse of the special matrix returned by "makeCacheMatrix"
## If the inverse has already been calculated retrieved the inverse from the cache memeory
cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        m <- x$inverse()
        
        ## If inverse already calculated return from cache.
        if (!.isnull(m) ){
          return(m)
        }
        
        ## retrieve the matrix
        data <- x$get()

        ## perform matrix multiplication
        m <- solve(data) %*% data
        
        ## get the inverse of the matrix
        x$inverse(m)
        
        ## return the matrix
        m
}

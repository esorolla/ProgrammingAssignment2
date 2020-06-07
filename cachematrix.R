## This function caches the calculation of a certain function on a
## matrix object.

makeCacheMatrix <- function(x = matrix()) {

    ## We initialize the matrix object    
    m <- NULL
    ## We set the matrix
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    ## We get the matrix
    get <- function() x
    ## We calculate the inverse of the matrix by using the function 'solve'
    setsolve <- function(solve) m <<- solve
    ## We get the inverse of the function as a list
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)

}


## The next function calculates the inverse of the matrix x
cacheSolve <- function(x, ...) {
    ## We assign the calculation of the inverse of the matrix x to the "local"
    ## variable m
    m <- x$getsolve()
    if(!is.null(m)) { ## If the matrix x has not been created, we don't
                      ## calculate the inverse, but we ouput the previously
                      ## stored value.
        message("getting cached data")
        return(m)
    }
    ## If the matrix x has been created, we output the inverse:
    data <- x$get()       ## We create a local variable: 'data' with the created
                          ## matrix x.
    m <- solve(data, ...) ## Here we calculate the inverse of data within this
                          ## environment.
    x$setsolve(m) ## Here we calculate the inverse within the father environment
                  ## (where x has been defined).
    m             ## Here we print the inverse
}
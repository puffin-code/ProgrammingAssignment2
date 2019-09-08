## Creates an matrix-like object that allows caching of the matrix inverse.  
## set and get opterations are limited to the matrix contents and/or the matrix
## inverse.
## Modelled after the code and intstructions provided here:
## https://www.coursera.org/learn/r-programming/peer/tNy8H/programming-assignment-2-lexical-scoping



## Set up the strucutre to support the matrix with a cache-able inverse
##  There are two important variables:
##       I = the matrix inverse
##       M = the matrix raw data
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    
    ## Reset the matrix and reset the inverse
    set_matrix <- function(y) {
        M <<- y
        INV <<- NULL  # can't use 'I', it is reserved
    }
    ## Set the inverse
    set_inverse <- function(inverse) INV <<- inverse
   
    ## return values
    get_inverse <- function() INV
    get_matrix <- function() M
    
    list(set_matrix = set_matrix, get_matrix = get_matrix,
         set_inverse = set_inverse,
         get_inverse = get_inverse)
}



## Retrieve a pre-computed inverse, or create one and save it.
## Expects X create with createCacheMatrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        this_inverse <- x$get_inverse()
        if(!is.null(this_inverse)) {
            message("getting cached data")
            return(this_inverse)
        }
        data <- x$get_matrix()
        
        #Assumes the data are invertible
        i <- solve(data, ...)
        x$set_inverse(i)
        i
}

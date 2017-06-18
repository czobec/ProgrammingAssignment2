## Matrix - Cache the invers of a matrix 
## Author: Chris Zobec

## The following functions allow the inverse of a matrix to cached 
## to avoid repeated computation.

## makeCacheMatrix - This function takes a matrix and creates a 'special'
##      matrix. 
##      The fuction returns a list containing functions that allow 
##      the user to :
##      1. set the value of the matrix
##      2. get the value of the matrix 
##      3. set the value of the inverse matrix
##      4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    
    Inv <- NULL
    
    ## funtion to set the value of the matrix
    set <- function(y) {
        x <<- y
        Inv <<- NULL
    }
    
    ## function to return the matrix
    get <- function() x
    
    ## function to set the value of the inverse matrix
    setInv <- function(solve) Inv <<- solve
    
    ## function to return the value of the inverse matrix
    getInv <- function() Inv
    
    ## output - list of functions
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}


## cacheSolve - The function takes a 'special' matrix created by the 
##      makeCacheMatrix function as the input. If there is no inverse
##      already cached in the 'special' matrix, it will calculate the 
##      inverse and store it in the 'special' matrix, and return the 
##      inverse matrix. If there is an inverse martix already cached,
##      it will return the inverse.

cacheSolve <- function(x, ...) {
    
    ## get the inverse matrix
    Inv <- x$getInv()
    
    ## check if the inverse matrix exists, 
    ## return the inverse if it exists and exit funtion.
    if(!is.null(Inv)) {
        
        message("getting cached inverse")
        return(Inv)
    }
    
    ## the follow steps are only executed if inverse martix is not cahced
    
    ## calculate the inverse matrix
    data <- x$get()
    Inv <- solve(data, ...)
    
    ## cache the inverse matrix
    x$setInv(Inv)
    
    ## return the inverse matrix
    Inv
}

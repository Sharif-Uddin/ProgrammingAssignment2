## The makeCacheMatrix function takes a numeric matrix as input and returns a
## list of functions - set, get, setinverse, getinverse. The functions set, get,
## setinverse, and getinverse are the so called "closures" as each of these
## enclose the environment of the parent function (makeCacheMatrix) and all its
## variables (x, inverse, and the other closures). Let z <- makeCacheMatrix().
## Then the enclosing environment of z$set can be checked by
## as.list(environment(z$set)).

## The functions set, get, setinverse, and getinverse either return a variable 
## defined in their parent environment or assign to a variable defined in the
## parent environment (x or inverse). The assignment in the parent environment
## is done by the superassignment operator <<- which specifically tells us that
## the variable being assigned to is defined in one of the parent environments.
## This control is possible because even though the execution environment
## changes everytime, the enclosing environment is constant. When a variable in
## the enclosing environment is modified by either set or setinverse, the
## variable is modified in the enclosing environment for all four functions -
## set, get, setinverse, getinverse.


## The makeCacheMatrix has one formal variable x (matrix), a local variable 
## inverse (matrix inverse) and four child functions - set, get, setinverse,
## getinverse. 
## The function set takes one arguemnt and assings the value of the
## argument to the variable x in its parent environment.
## The funtion get returns the variable x defined in its parent environment.
## The function setinverse takes one argument and assings it to the variable 
## inverse in its parent environment.
## The funtion getinverse returns the variable inverse defined in its parent 
## environment.
## makeCacheMatrix returns a list of funtions - set, get, setinverse, getinverse.
## The variables x and inverse are stored in the enclosing environment of
## these functions.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y){
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(x_inverse) inverse <<- x_inverse
    getinverse <- function() inverse
    list(set = set, get = get, 
         setinverse = setinverse, 
         getinverse = getinverse)

}


## The function cacheSolve takes the list of functions returned by makeCacheMatrix
## as input. Then the inverse of the matrix is stored in the enclosing environment
## of the funtions is retrieved using the getinverse() function. It the 
## retrieved x_inverse is not NULL then cacheSolve returns the retrieved 
## inverse of the matrix. Otherwise the numeric matrix is retrieved using
## the get() function and then the solve function computes the inverse. 
## The computed inverse is updated in the enclosing environment of the 
## list of functions using the setinverse() function. Then finally cacheSolve
## returns the computed matrix inverse. 

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    x_inverse <- x$getinverse()
    if(!is.null(x_inverse)) {
        message("Getting cached data")
        return(x_inverse)
    }
    data <- x$get()
    x_inverse <- solve(data, ...)
    x$setinverse(x_inverse)
    x_inverse
}

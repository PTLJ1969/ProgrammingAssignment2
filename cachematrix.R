## The first function in the file, makeCacheMatrix() creates a special "matrix" object capable of caching its inverse.

## The second function, cacheSolve() computes the inverse of the "special matrix" returned by makeCacheMatrix() 
## if this inverse has  been cached previously, however it retrieves the cached inverted matrix.

##====================================================================================================================
## Example Usage:

## Create an invertible matrix:
## > x <- matrix(rnorm(16),4,4)

## Pass the invertable matrix to makeCacheMatrix and assign the result (the inverted matrix) to the special "matrix":
## > myMatrix <- makeCacheMatrix(x)

## Note 'myMatrix' above is the special caching "matrix" object created by makeCacheMatrix(x) 
## You can check the content by running the command myMatrix$get()

## Pass the special "matrix" to cacheSolve:
## > cacheSolve(myMatrix)
## The inverted matrix will be returned

## Repeat the above command:
## > cacheSolve(myMatrix)
## The cached inverted matrix will be returned along with the message "getting cached data"
##====================================================================================================================

## The makeCacheMatrix function below creates a special "matrix" object capable of caching its inverse
 
makeCacheMatrix <- function(x = matrix()) {    ## incoming matrix x passed as arg
    
    cacheMatrix <- NULL                        ## initialise x (the incoming matrix)

    ## Define mutator and accessor methods (set,get,setsoln,getsoln):
    set <- function(y) {        
        x <<- y                 ## Assign input argument to the 'x' object in parent environment 
        cacheMatrix <<- NULL    ## Set 'cacheMatrix' object in the parent environment to NULL    
    }

    ## getter for matrix x
    get <- function() x        

    ## As cacheMatrix is defined in the parent environment. this setter assigns the input argument to the value of cacheMatrix in the parent environment
    setsoln <- function(solve) cacheMatrix  <<- solve

    ## getter for cached matrix 
    getsoln <- function() cacheMatrix


    ## each of the above methods is a named element in a list() enabling functions to be accessed by name...i.e. 'x$getsoln()':

    list(set  = set, get = get,
             setsoln = setsoln,
             getsoln = getsoln)
}


## The cacheSolve function below computes the inverse of the "special matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated, and the input matrix is unchanged, cacheSolve retrieves the existing inverse from the cache

cacheSolve <- function(x, ...) {

    ## retrieve a solved (inverted) matrix from the object passed in as the argument:
    cacheMatrix  <- x$getsoln()
    
    ## If retrieval succeeds i.e. the cached inverse exists, return it and print message:
    if(!is.null(cacheMatrix)) {
        message("getting cached data")
        return(cacheMatrix)
    }

    ## If retrieval fails i.e. the cached inverse doesn't exist,
    ## get the matrix from the input object:
    data <- x$get()

    ## calculate the inverse:
    cacheMatrix <- solve(data, ...)

    ## use input object's setsoln() to set the inverse in the input object:
    x$setsoln(cacheMatrix)

    ## return the inverted matrix to the parent environmetnt:
    cacheMatrix

}


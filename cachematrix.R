## Overall, the two functions are used for calculating the inverse
## of a given matrix. The functions are constructed such that
## inverse of a given matrix is calculated only once in the 
## first call. In the later calls, the already stored value of the 
## inverse is returned which saves a lot of computational time
## and resources.

## The function makeCacheMatrix takes a matrix and turns it into
## a R object. This R object has a list of four functions inside it.
## Once we have the R object then 
## we can access any of the four functions that this object contains
## using usual method of accessing a list element. For example, suppose we 
## create a makeCacheMatrix object called "cm" for a given matrix
## "m" (cm <- makeCacheMatrix(m)), then we can access the getinverse
## function by calling cm$getinverse(). Note that the bracket at the
## end of the function tells R that it is a function call and 
## is mandatory


makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## The function cacheSolve takes a makeCacheMatrix object as its 
## input and returns the inverse of the matrix (m) that was used 
## to form the makeCacheMatrix object (cm). It first checks if 
## the matrix inverse is already stored in the object itself. 
## if it is already stored then it simply accesses the stored 
## value and returns it. In case, the matix inverse is not 
## stored then it calculates the matrix inverse and returns 
## the inverse of the matrix. In-between, it also  
## sets the value of the inverse of the matrix inside 
## makeCacheMatrix object (cm) using setinverse() function .


cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setinverse(inv)
    inv
}



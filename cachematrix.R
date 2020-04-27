## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## the makeCacheMatrix function cretaes a special matrix object that can cache its inverse 
makeCacheMatrix <- function(a = matrix()) {
        inv <- NULL                            
    set <- function(y) {                    
        a <<- y                             
        inv <<- NULL                        
    }
    get <- function() a                     
    setinverse <- function(inverse) inv <<- inverse  
    getinverse <- function() inv                     
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) 

}


## Write a short comment describing this function
##the cacheSolve function will get the inverse from the cache if the inverse has already been calculated and the matrix has not changed

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- a$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- a$get()
    inv <- solve(data, ...)
    a$setinverse(inv)
    inv
}

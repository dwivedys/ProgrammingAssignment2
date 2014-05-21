## makeCacheMatrix creates the matrix object on which we wish to perform the inverse operation that we want to cachce
## It contains the methods to appropriately get, set, getInv and setInv of the matrix

## First we create the matrix object with appropriate get set methods; this is where lexical scoping is used to store the Inverse of 
## where the inv variable in the parent frame is set from within the setInv function

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
  
  set <- function(m1) {
    m <<- m1
    inv <<- NULL
  }
  
  get <- function() m
  
  setInv <- function(inverse) inv <<- inverse
  getInv <- function() inv
  
  list(set = set, get=get, setInv = setInv, getInv = getInv)
  

}


## This function computes the inverse and caches it for later reuse. Calling CacheSolve on the cacheable matrix object checks to see
## if cached version is available; if not then it computes the inverse and stores it in the inv variable. This is then passed through
## setInv function of the cached matrix object and stored in the makeCacheMatrix object variable Inv for later reuse

cacheSolve <- function(x, ...) {
          inv <- m$getInv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat <- m$get()
  inv <- solve(mat)
  m$setInv(inv)
  inv

}

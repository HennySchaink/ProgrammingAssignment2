
## This routine consisting of 2 functions returns 
## the inverse of numeric matrix x. 
## If the contents of the matrix has not changed, 
## the inverse matrix is looked up in the cache


## For numeric matrix x, next function performs the following operations:
## 1) set : sets the values of the matrix x in the cache
## 2) get : gets the values of matrix x
## 3) setinv: sets the values of the inverse matrix
## 4) getinv: gets the values of the inverse matrix
## The return value is a list containing the information about 
## set, get, setinv and getinv

makeCacheMatrix <- function(x = numeric()) {
    m<-NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setsolv <- function(solve) m <<- solve
    getsolv <- function() m
    list(set = set, get = get,
               setsolv = setsolv,
               getsolv = getsolv)
}




## This function checks whether an inverse matrix can be 
## found in the cache (m unequals NULL). If so the inverse 
## matrix is extracted from the cache. 
## Else: the inverse is calculated using the data

cacheSolve <- function(x, ...) {
    m <- x$getsolv()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolv(m)
    m
}

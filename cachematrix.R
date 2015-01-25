
## The following functions calculate the matrix inverse and saves it to the cache. Next time when the user 
## tries to calculate the matrix inverse, the previously saved value is returned instead of repeating the calculation.
## To test this code you can use below example after sourcing this code  -> 
## x <- matrix(1:4,2,2)
## cacheSolve(makeCacheMatrix(x))
## Expected output is as below  -> 
##        [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

## This function "makeCacheMatrix" creates a special "matrix" object, which is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  ## create a matrix object x and some associated sub-functions/methods
  
  ## define the cache m
  m <- NULL
  set <- function(y) {
    x <<- y ## assign the input matrix y to the variable x in the
    ## parent environment
    m <<- NULL ## re-initialize m in the parent environment to null
  }
  get <- function() x ## return the matrix x
  setinverse <- function(inverse) m <<- inverse ## set the cache m equal
  ## to the inverse of the matrix x
  getinverse <- function() m ## return the cached inverse of x
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The following function "cacheSolve" calculates the inverse of the special "matrix" created with the above function "makeCacheMatrix".
## However, it first checks whether the inverse has already been caclulated or not. If already calculated, it gets the inverse from the cache
## and skips the computation. Otherwise, it calculates the matrix inverse and sets the value of the inverse in the cache via the 'setinverse' function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of matrix 'x'
  
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data : inverse of the matrix")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

## Hi Peer, thanks for taking the time to grade my assignment. 

## These function create the matrix with inverse cached
## and allow for calling of the inverse without recalculation

## Creates a matrix object with methods for setting, viewing, 
## calculating inverse and viewing inverse.
makeCacheMatrix <- function(x = matrix()) {
  
  stopifnot(nrow(x) == ncol(x)) # Matrices that are not square do not have an inverse, so let's throw an error.
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Caches the inverse of the matrix if not already calculated
## otherwise, retrieves calculated inverse with a message

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}

x <- matrix(c(1:4), ncol = 2)
# > x
#      [,1] [,2]
# [1,]    1    3
# [2,]    2    4
x <- makeCacheMatrix(x = x)
# > x$getInverse()
#      [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
cacheSolve(x)
#      [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
x$getInverse()
# > x$getInverse()
#      [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5

# Now the inverse is cached inside of the object _x_, yay!

# We should see a message telling us it's getting cached data for the next time we call cacheSolve(x)

cacheSolve(x)
# getting cached data
#      [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5

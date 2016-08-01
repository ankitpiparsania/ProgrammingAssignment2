## There are two parts of this program. The first part aims to
## read the martix, store the values in cache, set the 
## initial value of inverse flag to NULL
## This also creates a list that stores the martix, 
## inverse flag and inverse of matrix
## The second part of the code takes the list created by
## makeCacheMatrix and checks for the inverse. If it exists,
## pulls inverse from cache else creates inverse and stores it
## cache

## Take matrix as input, create list and set inverse flag as NULL

makeCacheMatrix <- function(x = matrix()) {

      z <- NULL
      setmat <- function(y) {
            x <<- y
            z <<- NULL
      }
      getmat <- function() x
      setinv <- function(mat_inv) z <<- mat_inv
      getinv <- function() z
      list(setmat = setmat, getmat = getmat,
           setinv = setinv,
           getinv = getinv)
}


## Take list crearted by makeCacheMatrix, check for inverse flag,
## If NULL then create inverse matrix and set inverse flag and
## if not, then pull the inverse from cache
## in both cases return the inverse of matrix back

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      z <- x$getinv()
      if(!is.null(z)) {
            message("Fetching the inverse from cached data")
            return(z)
      }
      
      z <- solve(x$getmat())
      x$setinv(z)
      z
}

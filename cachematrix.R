makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL                              #initializing inverse as null
     set <- function(y) {
          x <<- y 
          inv <<- NULL
     }
     get <- function() x                      #function to get matrix x
     setinverse <- function(inverse) inv <<- inverse
     getinverse <- function() inv
     list(set = set, get = get, 
     setinverse = setinverse, getinverse = getinverse)
}

cacheSolve <- function(x, ...) {              #gets cache data
     inv <- x$getinverse()
     if(!is.null(inv)) {                      #check if inverse is null
         message("getting cached data")
         return(inv)
     }
     data <- x$get()
     inv <- solve(data, ...)                  #calculates inverse of matrix
     x$setinverse(inv)
     inv
}

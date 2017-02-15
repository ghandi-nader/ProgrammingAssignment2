## Write a short comment describing this function
## this function has 4 functionality. every time it is called, it clear m
## m is the global cached value of the inversed matrix
## set is used to set new matrix
## get is used to get the current matrix 
## setinverse to set the new inverse
## getinverse to get the new inverse

makeCacheMatrix <- function(x = matrix()) {

              m <- NULL
            
              set <- function(y) {
   
                                   x <<- y
                                   m <<- NULL
                                  }
 
              
              get <- function() x
 
              setinverse <- function(solve) m <<- solve
              
              getinverse <- function() m
  
              list(set = set, get = get,
                   setinverse = setinverse,
                   getinverse = getinverse)
  
                                          }


## Write a short comment describing this function
## first the function get the matrix inverse cached(if any) in makeCacheMatrix and set it to m
## then it test if m is different than null that is mean inverse matrix is cached then it return m
## if m is null then it calculate the inverse of the matrix store it in m and return it

cacheSolve <- function(x, ...) {
              m <- x$getinverse()
              
              if(!is.null(m)) {
                message("getting cached data")
                return(m)
              }
              
              data <- x$get()
  
              m <- solve(data, ...)
  
              x$setinverse(m)
  
              m
  
  
}




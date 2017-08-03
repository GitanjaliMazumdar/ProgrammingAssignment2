
## The code consists of functions which cahces the inverse of a matrix so that it need not be repeatedly 
## calculated as matrix inversion is a costly computation 
## functions do

## The first function is the makeCacheMatrix function which creates a special matrix i.e. a list which sets
## value of the matrix, gets its value then sets the value of its inverse & finally gets the inverse. 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
          x <<- y
          inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}

## The second function first checks that whether the matrix inverse has already been calculated if yes then 
## gets the value from the cache & will not compute the same. If not then it will compute the matrix inverse

cacheSolve <- function(x, ...) {
  inv<- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  } 
  else{
    data <- x$get()
    inv <- solve(data)
    x$setInverse(inv)
    inv
  }
  
}

# example along with the output
# # using matrix x
#   x <- matrix(1:4, nrow=2, ncol = 2)
# # makeCacheMatrix function creates the special matrix
#   z <- makeCacheMatrix(x)
#   # printing the matrix
#     z$get()
#     [,1] [,2]
#     [1,]    1    3
#     [2,]    2    4
#   # its inverse should be NULL
#     z$getInverse()
#     NULL
# # cacheSolve function computes the inverse matrix in the first run
#   cacheSolve(z)
#   [,1] [,2]
#   [1,]   -2  1.5
#   [2,]    1 -0.5
#   # In the second run it will return the cached value
#   cacheSolve(z)
#   getting cached data
#   [,1] [,2]
#   [1,]   -2  1.5
#   [2,]    1 -0.5

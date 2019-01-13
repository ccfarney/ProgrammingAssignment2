## Sometimes it is necessary to create an inverse Matrix in R, but this can requires a lot of processing power.
## By caching the results of an inverse matrix, it makes it easier than running the entire computation repeatedly.

## The first step is to make the inverse matrix that is assigned to makeCacheMatrix, which I modeled after the example
## in the prompt.

makeCacheMatrix <- function(x = matrix()) {
  inv= NULL
  set= function(y) {
    x <<- y
    inv <<- NULL
  }
  get= function()x
  setinv= function(inverse) inv <<- inverse
  getinv= function () inv
  list(set=set, 
       get=get, 
       setinv=setinv, 
       getinv=getinv)
}


## The second step is to create a function that calls to the stored results of the original cached matrix, if available.

cacheSolve <- function(x, ...) {
  inv = x$getinv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data = x$get()
  inv = solve(data, ...)
  x$setinv(inv)
  return(inv)
}

## I like to test my code if possible, so here it is:
## I will create a test matrix that is 2x2 matrix will values 1 thru 4
test <- matrix(c(1:4),2,2)
##Then I will run the makeCacheMatrix and cacheSolve function I created
testmatrix <-makeCacheMatrix(test)
cacheSolve(testmatrix)

## And is all goes well, when I run the cacheSolve again, I should receive the message "getting cached data"
## which tells me that it is not running the inverse of the original matrix, but is calling on the cached data
cacheSolve(testmatrix)

## function creates special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x=matrix())   {
  i <- NULL # initial to Null
  set <- function(y) {
    x<<-y
    i<<-NULL
    } # flag inverse to Null to be recalculated later if value changed
  get <- function() x
  setinv <- function(invm) i <<- invm #direct set inverse matrix
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## calc inverse of matrix x if not in cache or use from cache
## returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
  ## Return inverse of matrix 'x'
  i <- x$getinv()
  if(!is.null(i))   {
    message("getting cached data")
    return(i)} #reuse calc'd value & return
  data <- x$get()
  i <- solve(data, ...) #calc inverse matrix
  x$setinv(i) #set inverse value via setinv function
  i
}
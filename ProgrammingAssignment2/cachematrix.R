makeCacheMatrix <- function(x = matrix()) {
  # set cache
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

cacheSolve <- function(x,...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached inverse matrix")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setinverse(m)
  m
}

m_matrix<-matrix(c(1,3,11,13),nrow=2,ncol=2)
cacheSolve(aa)
cacheSolve(aa)

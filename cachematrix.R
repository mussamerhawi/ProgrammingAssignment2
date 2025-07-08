## The following pair of functions compute and/or cache the inverse of a matrix


## The first function, makeCacheMatrix creates a special "matrix" object, which is really a list containing a function 
#to:

    #set the inverse of the matrix

   #get the inverse of the matrix

   #set the matrix

  #get the matrix

makeCacheMatrix <- function(x = matrix()) {
  inve<-NULL
  set<-function(y){
    x<<-y
    inve<<-NULL
  }
  get <- function() x
  setinverse <- function(inverse) inve <<- inverse
  getinverse <- function() inve
  list(set=set,
       get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}


## This function computes the inverse of the special "matrix" 
#returned by makeCacheMatrix above. If the inverse has already 
#been calculated (and the matrix has not changed), then the cachesolve 
#should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inve <- x$getinverse()
  if(!is.null(inve)) { 
    message("getting cached inverse matrix")
    return(inve)
  } 
  data <- x$get()
  inve <- solve(data, ...)
  x$setinverse(inve)
  inve
}
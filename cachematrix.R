# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
# initialize x as an argument of the function as an empty numeric vector
makeCacheMatrix <- function(x = numeric()){
  
  # declare and intitalize the variable m
  m <- NULL
  
  # set the value of the matrix
  set <- function(y){
    
    # assign y to object x in the parent env
    x <<- y
    m <<- NULL
  }
  
  # retrieve x from parent env, get value of matrix
  get <- function() x
  
  #set value of inverse matrix, assign input argument to the value of m in the parent env
  setinverse <- function(solve) m <<- solve
  
  #retrieve m from parent env, get value of inverse matrix
  getinverse <- function() m
  
  #return functions within a list to the parent environment to be accessible to cacheSolve
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

# cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  
  #has the inverse already been calculated?
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  
  #calculate the inverse of the matrix
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
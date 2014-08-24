# 
# First I apologize for my little english ...
# detailed comments are within the functions
# 
# More generally both functions are working like the both given as an example
# I detailed the step by step comments to ensure that i understood every single step


makeCacheMatrix <- function(x = matrix()) {
  #this is quite the same function that the given example makeVector()
  #the only differences are 
  #        - if arguments are omitted the default value for x is an empty matrix 
  #        - we use the solve function instead of mean
  #        - i is used to store the inverse of the x matrix (as m was used to store the mean in the example makeVector())
  i <- NULL
  #the set function will take a matrix as an argument and store it in x, we have to set also i with NULL, if we don't we could return 
  #a false result (the inverse of the matrix stored previously in x)
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  #the get function returns the initial matrix
  get <- function() x
  #the setinverse function stores the matrix passed in argument in i
  setinverse <- function(inverse) i <<- inverse
  #the getinverse function returns the matrix stored in i (or NULL if not set)
  getinverse <- function() i
  #at last we return 
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}

cacheSolve <- function(x, ...) {
  # first we try to retrieve cached data with the embedded function of the special matrix : getinverse()
  i <- x$getinverse()
  if(!is.null(i)) {
    #if m is not null we successfully retrieved cached data we just have to return the cached result
    #note that the return instruction stops the execution here
    message("getting cached data")
    return(i)
  }
  #so, if we execute the following part of the function it indicates that there was no chached data
  #then we have to compute the inverse of the matrix
  
  #first we get the matrix to invert and store it in a matrix called data
  data <- x$get()
  
  #then we invert it with the solve() function and store the result in a matrix called inverted
  inverted <- solve(data)
  
  #we store the result in the cache of the "special matrix" with its embedded function setinverse()
  x$setinverse(inverted)
  #and at last we return the inverted matrix
  inverted
}

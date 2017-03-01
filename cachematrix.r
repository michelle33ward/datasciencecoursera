#The first function, makeCacheMatrix creates a special "vector", which is really a list of functions to
#   set the values of a matrix
#   get the values of matrix
#   set the value of the inverse of a function
#   get the value of the inverse of a function

makeCacheMatrix <- function(x = matrix()) { #pass makeCacheMatrix x which is a matrix
  inv <- NULL                               # Setting inv=NULL shows inv is empty 
  set <- function(y) {                      # function that initializes the matrix and inverse
    x <<- y
    inv <<- NULL
  }
  get <- function() x                        #This function gets the matrix
  setInverse <- function(inverse) inv <<- inverse  #this function sets the inverse
  getInverse <- function() inv                       *this function will get the inverse  
  #pass back a list that contains the four functions defined above.  
  list(set = set,get = get,setInverse = setInverse,getInverse = getInverse) 
}
#CacheSolve will check to see if the inverse of a matrix has been done already.  If it has then it will 
#not recompute the inverse but instead get it from cache.  If the inverse has not been computed, the 
#inverse will be computed and stored in inv.

cacheSolve <- function(x, ...) {
  # Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()   #inverse is set by calling getInverse function
  if (!is.null(inv)) {     #if the inverse is already calculated  if so return the inverse from cache
    message("Not computing inverse, getting cached data instead")
    return(inv)
  }
  matrix <- x$get()           #the inverse hasn't been called so get the matrix
  inv <- solve(matrix, ...)   #compute the inverse of the matrix 
  x$setInverse(inv)           #Puts the inverse in cache. 
  inv
}

# makeVector function creates vector

makeVector <- function(x = numeric()) {
  m <- NULL
  # vector value setting
  set <- function(y) {    
    x <<- y
    m <<- NULL
  }
  # get vector value
   get <- function() x
  # Set mean value 
  setmean <- function(mean) m <<- mean
  # Get mean value
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

# cachemean function created with the function stated previously
cachemean <- function(x, ...) {
  m <- x$getmean()
#Chedk if the mean calculated---> retrurn mean
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()

# if mean not calculated (calculate data mean) 
  m <- mean(data, ...)
 
# Set mean value  
  x$setmean(m)
  m
}

## function that cache the inverse of a matrix.
## Function to create "matrix" that caches its inverse.


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(solveMatrix) inv <<- solveMatrix
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Function to compute the inverse of the matrix gotten from makeCacheMatrix.
cacheSolve <- function(x, ...) {
  ## Return inverse matrix of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv      
}






  
  




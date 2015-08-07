## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  #sets inverted matrix to NULL
  inv<- NULL
  #sets matrix to the vaule given and changes inverted matrx to NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  #function that will return the matrix
  get <- function() x
  #function that will set the inverse
  setinv <- function(inverse) inv <<- inverse
  #function that will get the inverse
  getinv <- function() inv
  #creates list of all values made/changed in this function
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  #Returns the value associated with the inverse in x
  inv<- x$getinv()
  #if the inverse is persent it returns the inverse and exits the function
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  #loads matrix into data
  data <- x$get()
  #calculates the inverse and sets it to inv
  inv <- solve(data, ...)
  #sets the inv in dataframe to be the inverse of the matrix given
  
  x$setinv(inv)
  #prints inverse
  inv
}

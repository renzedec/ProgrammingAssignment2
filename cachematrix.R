## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
## This function sets and gets the value of a matrix and sets and gets the value of its inverse
  invmat <- NULL              #initialize this variable
  set <- function(y) {
    x <<- y                  #matrix value on the "different environment"
  invmat <<- NULL
  }
  get <- function() x                                 #get the value of the matrix
  setinvmat <- function(inverse) invmat <<- inverse   #set the value of the inverse of the matrix
  getinvmat <- function() invmat                      #get the value of the inverse of the matrix
  list(set = set, get = get,                          
       setinvmat = setinvmat,
       getinvmat = getinvmat)                         #create a list of all those data
}



## Write a short comment describing this function

## The function takes the output of the previous function, checks 
## if there are stored values on the cache and solves accordingly

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invmat <- x$getinvmat()
  if(!is.null(invmat)) {              #if inverse matrix is not NULL
    message("getting cached data")
    return(invmat)                    #return the cached value
  }
  data <- x$get()
  invmat <- solve(data, ...)
  x$setinvmat(invmat)
  return(invmat)
}
  

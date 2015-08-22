## Put comments here that give an overall description of what your
## functions do

## The functions create an inverse matrix to an invertible matrix. When running the solve command, the function checks whether the invertible matrix has already been inverted. If so, it only returns the inverse matrix which has been stored as solution.

## Write a short comment describing this function

## makeCacheMatrix is a list of 4 functions used to get the invertible matrix (get), reset the invertible matrix if it has to be changed (set), set the inverse solution when cacheSolve is run (setsolve), and return the inverse solution into cacheSolve (getsolve).

makeCacheMatrix <- function(x = matrix()) {         #vector of 4 functions, the argument being the invertible matrix
  s <- matrix()                                     #inverse matrix
  set <- function(y) {                              #function re-setting the invertible matrix x to y as well as the inverse matrix back to an empty matrix s
    x <<- y
    s <<- matrix()
  }
  get <- function() x                               #function returning the invertible matrix
  setsolve <- function(solve) s <<- solve           #function setting the solution inverse matrix to s (activated by the cacheSolve function after the inverse is calculated)
  getsolve <- function() s                          #function returning the inverse matrix (activated by the cacheSolve function before the inverse is calculated to check if it has already been calculated)
  list(set = set, get = get,                        #list to store the 4 functions
       setsolve = setsolve,
       getsolve = getsolve)
}

## Write a short comment describing this function

## cacheSolve returns the inverse solution (if it has already been calculated) or calculates the inverse 
cacheSolve <- function(x, ...) {      #function calculating the inverse or retreiving it from cache, the argument being the makeCacheMatrix function calculated on the invertible matrix
  s <- x$getsolve()                   #retreives the inverse matrix
  if(!is.na(s[1,1])) {                #checks whether the inverse matrix has already been calculated
    message("getting cached data")    #if so, it returns a message and the cache inverse
    return(s)
  }
  data <- x$get()                     #if not, it retrieves the invertible matrix from makeCacheMatrix
  s <- solve(data, ...)               #it then solves it
  x$setsolve(s)                       #it then sets the inverse as solution into makeCacheMatrix
  s                                   #it finally returns the solution
}


##************************************************************************************************
##Matrix inversion is usually a costly computation and there may be some benefit to caching the 
##inverse of a matrix rather than computing it repeatedly (there are also alternatives to matrix 
##inversion that we will not discuss here). The assignment is to write a pair of functions that 
##cache the inverse of a matrix.
##
##Computing the inverse of a square matrix can be done with the solve function in R. 
##For example, if X is a square invertible matrix, then solve(X) returns its inverse.
##
##For this assignment, it is assumed that the matrix supplied is always invertible.
##
##************************************************************************************************

#makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##set the value of the matrix
##get the value of the matrix
##set the inverse value of the matrix
##get the inverse value of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse<- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


##cacheSolve: This function computes the inverse of the special "matrix" returned by 
##makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), 
##then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}

##TEST RUN 1
##> setwd("/Users/Manny/Documents/MESTA/DataScience/Rprojects/ProgrammingAssignment2")
##> getwd()
##[1] "/Users/Manny/Documents/MESTA/DataScience/Rprojects/ProgrammingAssignment2"
##> source("cachematrix.R")
##> x = rbind(c(2, -1/2), c(-1/2, 2))
##> m = makeCacheMatrix(x)

##No cache in the first run
##> cacheSolve(m)
##[,1]      [,2]
##[1,] 0.5333333 0.1333333
##[2,] 0.1333333 0.5333333

##Taking the value from the cache in the second run
##> cacheSolve(m)
##getting cached data
##[,1]      [,2]
##[1,] 0.5333333 0.1333333
##[2,] 0.1333333 0.5333333

##*************************************************

##TEST RUN 2
##> x = rbind(c(3, -1/3), c(-1/3, 3))
##> m = makeCacheMatrix(x)

##No cache in the first run
##> cacheSolve(m)
##[,1]   [,2]
##[1,] 0.3375 0.0375
##[2,] 0.0375 0.3375

##Taking the value from the cache in the second run
##> cacheSolve(m)
##getting cached data
##[,1]   [,2]
##[1,] 0.3375 0.0375
##[2,] 0.0375 0.3375




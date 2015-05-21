## Alex Gómez Garrido
## 22/05/2015
## Coursera R programming: Programming Assignment 2

## This module is able to create a special matrix object,
## where the matrix is sotred, but also its inverse can be calculated and stored.

## makeCacheMatrix: Argument should be a Matrix
## This function creates this special matrix object.
## The methods implemented in this object are:
## set: Store the matrix.
## get: Return the matrix.
## setInverse: Set the inverse matrix variable with a matrix (not necessary the inverse matrix)
## getInverse: Return the matrix stored in the inverse matrix variable

makeCacheMatrix <- function(x = matrix()) {
  if(!is.matrix(x)){
    stop('First Argument should be a Matrix')
  }
  m<-NULL
  set <- function(y){
    x<<-y
    m<<-NULL
  }
  get <- function() x
  setInverse<-function(solve) m <<-solve
  getInverse<-function() m
  list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}

## CacheSolve: Argument should be a Matrix
## This function does the inverse of a quadratic matrix with determinant different than zero.
## If the matrix is the special matrix object created in this module it first look
## if the inverse matrix is already stored in the cahce of this object and return this,
## but if it is not there, it calculates and return the inverse matrix.


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m<-x$getInverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data<-x$get()
  m <- solve(data,...)
  x$setInverse(m)
  m
}

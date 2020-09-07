#The first function makecacheMatrix create a special"matrix" object that can cache its inverse.
##Then the function cachesolve computes the inverse of the special "matrix" returned by makecacheMatrix above.
###If the inverse has already been calculated then the cachesolve retrives the inverse from the cache
####The function makecacheMatrix creates a matrix object.This object is capable of caching its own inverse.

makecacheMatrix<-function(x=matrix()){
  inv<-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function()x
  setinverse<-function(inverse) inv<<-inverse
  getinverse<-function() inv 
    
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}

##The function cachesolve here computes the inverse of the matrix object that is returned from the above function

##Also,if the inverse has alraedy been calculated then cacahesolve retrives the inverse from the cache.

cachesolve<-function(x,...){
  inv<-x$getinverse()
  if(!is.null(inv)){
    message("getting cached matrix inverse")
    return(inv)
  }
data<-x$get()
inv<-solve(data,...)
x$setinverse(inv)
inv
}



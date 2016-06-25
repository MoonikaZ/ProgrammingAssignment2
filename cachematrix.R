## Put comments here that give an overall description of what your
## functions do:
## Functions makeCacheMatrix and cacheSolve calculate the inverse of the matrix and cache the results

## Write a short comment describing this function:
## makeCacheMatrix creates a "special vector", list containing function to set matrix, get matrix,
## set inverse matrix, get inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  
  m<-NULL                               #sets inverse matrix to NULL
  
  set<-function(y) {                    #sets the matrix x to matrix y and resets inverse matrix to NULL
    
    x<<-y
    m<<-NULL
    
  }
  
  get<-function()x                      #returns matrix
  
  setinverse<-function(solve) m<<-solve #sets inverse matrix
  
  getinverse<-function() m              #returns inverse matrix
  
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse) # lists four above functions
}


## Write a short comment describing this function:
## Function cacheSolve checks first if inverse matrix was calculated.
## If it was, then it gets inverse matrix from the cache without doing computation, 
## If it wasn't, then it calculates inverse matrix and sets it in the cache using setinverse function.

cacheSolve <- function(x, ...) {
  # 
  
  m<-x$getinverse()                     #assigns inverse matrix from cache to variable m (if not in cache then NULL)
    if(!is.null(m)) {                   #then if inverse matrix exists in cache, returns the below message and returns inverse matrix 

    message("getting cached data")
    return(m)
  }
  original<-x$get()                     #assigns matrix
  m<-solve(original,...)                #calculates matrix inverse to original
  x$setinverse(m)                       #sets inverse matrix in the cache
  m
}

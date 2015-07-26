#Rod L E
#functions to store on cache the inverse of matrix to avoid 
#doing the same calculations over and over again

makeCachem <- function(x = matrix()) { 
  #initialize value
  minv <- NULL                     
  #delcare another function set where the value will be cached
  set <- function(y) {                      
    x <<- y
    #change the value in case the matrix changed
    minv <<- NULL              
  }
  #gets the value of the inv
  get <- function() x                           
  #solving inv
  setinv <- function(solve) minv <<- solve 
  #gets the inv     
  getinv <- function() minv        
  #returns values to function       
  list(set = set, get = get,                    
       setinv = setinv,
       getinv = getinv)
}

#argument called used to get the cache
cacheSolve<- function(x, ...) {                 
  minv <- x$getinv()
  #if the inverse exists gets it
  if(!is.null(minv)) {                 
    message("getting cached data")
    return(minv)
  }
  #if the cache not found, is calculated, stored and brought back
  data <- x$get()                               
  minv <- solve(data, ...)
  x$setinv(minv)
  minv
}

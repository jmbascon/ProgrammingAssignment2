
## Programming Assignment 2

## Calculating the inverse for non-squared and square matrices is done using library(MASS)
## set, get, setinv, and getinv is used

library(MASS)
makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL              #sets inverse as NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  } 
  get<-function()x          # getting matrix x
  setinv<-function(inverse)inv<<-inverse
  getinv<-function(){
    inver<-ginv(x)
    inver%*%x             # gets the inverse of the matrix
  }
  list(set = set, 
       get = get,
       setinv = setinv,
       getinv = getinv)
}


## the cache data is found using this function

cacheSolve <- function(x, ...)         # finds the cache data
{
  inv<-x$getinv()
  if(!is.null(inv)){                  # checks if the inverse is null
    message("Cache Data:")
    return(inv)                   # returns to the inverse value
  }
  data<-x$get()
    inv<-solve(data,...)        # solves the inverse value
  x$setinv(inv)
  inv

}

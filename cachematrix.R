## This Programme is to calculate the inverse of a matrix
## It it had been worked out, the function returns the cached value
## if it hadn't been worked out,the function will calculate it.

## The test code could be:
## a<-matrix(c(1,1,-1,1),nrow=2,ncol=2)  %The test matrix
## b<-makeCacheMatrix(a)
## cacheSolve(b)
## it would return the right answer


## Function makeCacheMatrix is for store the matrix and inversed one

makeCacheMatrix <- function(x = matrix()) {
    m<-NULL
    set<-function(y)
    {
      x<<-y
      m<<-NULL
    }
    get<-function()
    {
      x
    }
    setInverse<-function(Inverse)
    {
      m<<-Inverse
    }
    getInverse<-function()
    {
      m
    }
    
    list(set=set,get=get,
         setInverse=setInverse,
         getInverse=getInverse)
}


## Function cacheSovle is to return the inversed matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'.
  
    m<-x$getInverse()
    if(!is.null(m))
    {
      message("getting cached data")
      return(m)
    }
    data<-x$get()
    m<-solve(data,...)
    x$setInverse(m)
    m
}

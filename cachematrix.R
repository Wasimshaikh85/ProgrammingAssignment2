library(MASS) #is used to calculate inverse of non squared as well as squared matrix 
makecachematrix <- function(x = matrix()) {
  inv <- NULL #initializing inverse as null
  set <- function(y) {
                  x <<- y
                  inv <<- NULL
                  }
  get <- function() x #Function to get matrix x
  setInverse <- function(inverse)inv <<- inverse
  getInverse <- function() {
                        inver<-ginv(x)
                        inver%*%x #function to obtain inverse of the matrix
                          }
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

cachesolve <- function(x, ...) #get cache data
  {
  inv <- x$getInverse()
  if(!is.null(inv)) { ##checking whether inverse is null
                message("getting cached data")
                return(inv) #return inverse value
  }
  data <- x$get()
  inv <- solve(data, ...) #calculate inverse value
  x$setInverse(inv)
  inv ##Return a matrix the is the inverse of x
}

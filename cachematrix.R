## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Based on a matrix received as a parameter, this function store the matrix and the inverse matrix
##  the internal functions can be used to write and retrieve the matrix and their correspondent inverse matrix value
## There are 4 internal functions:
##  set - store the matrix passed as a parameter and set the inverse matrix value to NULL
##  get - retrieve the matrix passed as a parameter from the "cache"
##  setinverse - store the inverse matrix value calculated in the "cache"
##  getinverse = retrieve the inverse matrix value from the "cache"
## The function return a list with the 4 functions

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL ## set inverse matrix value to NULL
  set<-function(y){ ## store matrix value
    x<<-y
    inv<<-NULL
  }
  get<-function() x ## retrieve matrix value
  setinverse <- function(inverse) inv<<-inverse ## store inverse matrix value
  getinverse <- function() inv ## retrieve inverse matrix value
  list(set=set, get=get, setinverse=setinverse,getinverse=getinverse) ## return a list with the functions
}


## Write a short comment describing this function
## Based on a matrix created with the "makeCacheMatrix" function, this function first use getinverse function to retrieve 
##  the inverse matrix value for the matrix. If the value exists then a message is presented to the user, explaining that
##  cached value will be used, and return the inverse matrix value for the user
## If a NULL value is returned, than the function uses the solve function to calculate the inverse matrix and than store
##  the value in the matrix passed as parameter, using the setinverse function, passing the calculate value and returning
##  to the user the value calculated

cacheSolve <- function(x, ...) {
  inv<-x$getinverse() ## retrieve inverse matrix value from the "cache"
  if (!is.null(inv)){ ## if value retrieved is not NULL, shows a message and return the value stored in the "cache"
    message("getting cached data")
    return(inv)
  }
  data<-x$get() ## retrieve the matrix 
  inv<-solve(data,...) ## calculate the inverse matrix using solve()
  x$setinverse(inv) ## store inverse matrix in the "cache"
  inv ## returns inverse matrix value
}

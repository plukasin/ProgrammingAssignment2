## There is a benefit in catching result of matrix inversion rather than 
## revere is each time even when it has not chnaged 

## makeCacheMatrix function creates a list containing 4 functions to
## 1 set the value of the matrix
## 2 get the value of the matrix
## 3 set the value of inverse of the matrix
## 4 get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  
  ## set local inv variable to NULL
  inv <- NULL
  ## 1 set the value of the matrix function 
  set <- function(y) {
    ## use y to set value of matrix x in enclosing environment
    x <<- y
    ## set inv in the enclosing environment to NULL 
    ## this deletes cached copy of inverse matrix 
    inv <<- NULL
  }
  ## 2 get the value of the matrix x function 
  get <- function() x
  
  ## 3 set the value of inverse matrix function
  ## assign to inv in enclosing environment the value of variable 'inverse' 
  setinverse <- function(inverse) inv <<- inverse
  
  ## 4 get the value of inverse matrix function
  ## it returns inv which is our cached inverse matrix
  getinverse <- function() inv
  
  ## list of 4 functions 
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve function returns the inverse of the matrix. 
## It first checks if the inverse has already been computed by checking 'inv' variable.
## If 'inv' is not null, it gets the result cached in 'inv'. 
## If 'inv' is null, it computes the inverse matrix, and sets the value in the cache 
## using setinverse function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x' 
  ## assign to 'inv'  value by excuting function getinverse()  
  inv <- x$getinverse()
  ## Option 1 'inv' is not null, print message
  if(!is.null(inv)) {
     message("getting cached data.")
    ## return inverse matrix 'inv' and exit function 
    return(inv)
    ## end for option 1 
  }
  ## Option 2 'inv' was null, assign matrix to variable 'data'
  ## by running get() function
  data <- x$get()
  ## inverse the matrix 'data' and assign it to 'inv'
  inv <- solve(data)
  ## run setinverse function which will set the value in cache 
  x$setinverse(inv)
  ## return 'inv' - which is our inversed matrix 
  inv
  ## end for option 2  
}


## Test: 
## p = rbind(c(1, 2), c(- 0 , 1))
## pit = makeCacheMatrix(p)
## cacheSolve(pit)

##       [,1] [,2]
## [1,]    1   -2
## [2,]    0    1

## Note: Will get an error if matrix cannot be solved - such as 2 X 3 matrix below
## p = rbind(c(1, 2,4 ), c(-1/4, 0 , 1)) 

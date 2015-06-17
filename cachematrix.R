## This program will exhibit the use of lexical scoping to find the inverse of a matrix using solve()

## This function creates a cache for the matrix to store the matrix.
##    1. set the value of the matrix
##    2. get the value of the matrix
##    3. set the value of the inverse of the matrix
##    4. get the value of the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) 
{
  m <- NULL
  set <- function(y)
  {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  setmatrix<-function(solve) m<<- solve
  getmatrix<-function() m
  list(set = set, get = get, setmatrix = setmatrix, getmatrix = getmatrix)

}


## The following function calculates the inverse of the given matrix.
## The matrix must be square and non-singular.

cacheSolve <- function(x, ...)
{
  m <- x$getmatrix()
  if(!is.null(m))
  {
    message("getting cached data")
    return(m)
  }
  matrix <- x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  
  m ## Return a matrix that is the inverse of 'x'
}

a <- makeCacheMatrix(x)
inverse <- cacheSolve(a)
inverse

## Example input 

x <- matrix(c(2,3,-1,4), nrow = 2, byrow = FALSE)

      

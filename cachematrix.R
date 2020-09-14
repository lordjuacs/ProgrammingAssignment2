## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL #creates a null variable in the inner scope
  setMatrix <- function(y) {
    x <<- y #cache the matrix
    inverse <<- NULL #set variable to null
  }
  getMatrix <- function() x #get the cached matrix  
  setInverse <- function(inv) inverse <<- inv #save cached in inverse
  getInverse <- function() inverse #get the saved cached inverse
  list(setMatrix = setMatrix,
       getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse) #this list is able to save all the previous functions
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  inverse <- x$getInverse() #gets the if-already-calculated inverse
  if (!is.null(inverse)) {#checks if has been compiled before 
    return(inverse)
  }
  else{
    matrix <- x$getMatrix() #get value of matrix
    x$setMatrix(matrix)#cache matrix
    inversa <- solve(matrix, ...)#solve the inverse
    x$setInverse(inversa)#cache the inverse
    inversa #return inverse
  }
        ## Return a matrix that is the inverse of 'x'
}

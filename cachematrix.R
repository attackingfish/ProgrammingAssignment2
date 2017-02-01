rm(list=ls())

################################################
## makeCacheMatrix
## Creates a special type of matrix to allow for
## the cacheing of the inverse.
## Useage:  myCachedMatrix <- makeCacheMatrix(myMatrix)
################################################

makeCacheMatrix <- function(x = matrix()) {

	inverseMat <- NULL

	# This set function isn't required.
      set <- function(y) {
                x <<- y
                inverseMat  <<- NULL
      }

      get <- function() x

      setInverse <- function(inverse) inverseMat <<- inverse

      getInverse <- function() inverseMat

        list(set = set, get = get,
             setInverse = setInverse ,
             getInverse = getInverse )

}


################################################
## cacheSolve 
## Checks whether the inverse has already been
## calculated, and then returns the cached value,
## or finds the inserve using the solve function
## Useage: cacheSolve (myCachedMatrix)
################################################

cacheSolve <- function(x, ...) {

     	  inv <- x$getInverse()

        if(!is.null(inv)) {
                message("getting cached inverse")
                return(inv)
        }

        data <- x$get()
        inv<- solve(data, ...)
        x$setInverse (inv)
        inv

}

myMatrix <- matrix(1:4,2,2)
myCachedMatrix <- makeCacheMatrix(myMatrix)

cacheSolve (myCachedMatrix)
cacheSolve (myCachedMatrix)

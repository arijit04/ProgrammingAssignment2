## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
## initialisation
		m <- NULL
		## storing the initial matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
		## retrieving the initial matrix
        get <- function() x
		## storing the solved inverse of the matrix
        setinverse<- function(inverse) m <<-inverse
		## retrieving the solved inverse of the matrix
        getinverse <- function() m
		## list of actions for the makeCacheMatrix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		m <- x$getinverse()
		## if cache exists, return cached data
        if (!is.null(m)) {
                message("getting cached data")
				## return the solution
                return(m)
        } else {
		## retrieve matrix data
                data<-x$get()
				## solve matrix inverse
                m <- solve(data)
                x$setinverse(m)
				## return the solution
                return(m)
        }
}

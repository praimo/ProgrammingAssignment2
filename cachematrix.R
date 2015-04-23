## R Programming - Programming Assignment 2
## Data Science Specialization Track
## Coursera - Johns Hopkins University

## Matrix inversion is usually a costly computation 
## especially for large matrices, so there is a potential 
## benefit to caching the inverse of a matrix rather than 
## compute it repeatedly. 

## These two functions are used to create a special object
## that stores a matrix and then caches the inverse of the matrix.

## The first function, makeCacheMatrix, creates a special "matrix"
## which is really a list containing a function to:
## 1. set the matrix
## 2. get the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
		# Initializes to NULL
        matrixinverse <- NULL
		
		# Sets the matrix
        set <- function(y) {
                x <<- y
                matrixinverse <<- NULL
        }
		
		# Gets the matrix
        get <- function() x
		
		# Sets the inverse of the matrix
    # Uses solve() to find the inverse of a matrix 
    # and cache it using a free floating variable
        setinverse <- function(solve) matrixinverse <<- solve
		
		# Gets the inverse of the matrix
        getinverse <- function() matrixinverse
		
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The second function, cacheSolve, computes the inverse of 
## the special "matrix" returned by makeCacheMatrix. If the inverse 
## has already been calculated (and the matrix has not changed),
## then cacheSolve retrieves the inverse from the cache. 

cacheSolve <- function(x, ...) {
		# Gets inverse from makeCacheMatrix
        matrixinverse <- x$getinverse()
		
		# Checks to see if inverse matrix has been calculated
		# If inverse has already been calculated, message is returned.
        if(!is.null(matrixinverse)) {
                message("getting cached data")
                return(matrixinverse)
        }
		
		# If not, get matrix
        data <- x$get()
		
		# Find the inverse of the matrix
        matrixinverse <- solve(data, ...)
		
		# Cache the result
        x$setinverse(matrixinverse)
		
		# Return the new inverse matrix
        matrixinverse
}

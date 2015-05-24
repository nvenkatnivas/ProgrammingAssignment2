##R Programming Course
##Programming Assignment 2
##Developer : N Venkat Nivas
##Date      : 24 May 2015
########################################

# makeCacheMatrix and cacheSolve are functions that are used to calculate the inverse of a matrix 
# and store it in the cache for future use. It saves the time while repeatedly 
# calculating the inverse of matrices.   


## Write a short comment describing this function

# makeCacheMatrix is used to create a list that contain the following
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse matrix
# 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    
    # set the inverse value of the matrix to null
    inverse.matrix <- NULL
    
    # store the input matrix in cache and the value of the inverse as null(Since, inverse is not calculated 
    # yet).
    setmatrix <- function(y) {
        x <<- y
        inverse.matrix <<- NULL
    }
    
    # function that returns the matrix in cache.
    getmatrix <- function() x
    
    # Function that sets the inverse value of the matrix in cache.
    setinverse <- function(inverse.value) inverse.matrix <<- inverse.value
    
    # Returns the inverse matrix stored in cache.
    getinverse <- function() inverse.matrix
    
    # creates a list that has the values of the functions setmatrix,getmatrix,setinverse and getinverse. 
    list(setmatrix = setmatrix, getmatrix = getmatrix, setinverse = setinverse, getinverse = getinverse)
    
}


# cacheSolve is used to solve the inverse of a matrix.
# checks if the inverse is present in the cache. 
# If yes then returns the cached data
#   Else calculates the inverse using solve() function.
#       Sets the value of the inverse in the cache
#       Display the inverse matrix

cacheSolve <- function(x, ...) {
    # retrieve the inverse value of the matrix from Cache.
    inverse.matrix <- x$getinverse()
    
    # check if the inverse value in cache has any value.
    # if not null then print the data in cache.
    
    if(!is.null(inverse.matrix)) {
        message("getting cached data")
        
        # terminate the execution of the function by returning the inverse matrix
        return(inverse.matrix)
    }
    
    # if inverse is not present in the cache then get the value of the matrix from cache.
    data <- x$getmatrix()
    
    # Calculate the inverse of the matrix using solve(matrix) function.
    inverse.matrix <- solve(data)
    
    # set the inverse value in cache.
    x$setinverse(inverse.matrix)
    
    # return the cache value to terminate the function.
    inverse.matrix
}


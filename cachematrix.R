## The following functions computes the inverse of a matrix, and stores 
#the results so it can be look at later without calculating it again. The
# function makeCacheMatrix is where the data is store (matrix and its inverse),
#and the function cachesolve its where the inverse matrix is calculated or
# retrieved from the other function. 


## This function creates a list that contains a function to:
#1 sets the matrix you want to inverse
#2 gets the matrix you want to inverse, is used in next function(cacheSolve)
# when the original matrix has not been solve (inverse), the cachesolve function
# takes the original matrix from here. 
#3 stores the inverse matrix when its calculated by the function cachesolve
#4 gets the inverse matrix, is where the cachesolve function checks to see if
# the inverse of the matrix has been calculated.
#This function has to be assign to a value, this value is used in the 
#next function (cachesolve) example: a<-makeCachematrix cachesolve(a).


makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        setmatrix <- function(y) {
                x <<- y
                m <<- NULL
        }
        getmatrix <- function() x
        setinvermatrix <- function(solve) inverse <<- solve
        getinvermatrix <- function() inverse
        list(setmatrix = setmatrix, getmatrix = getmatrix,
             setinvermatrix = setinvermatrix,
             getinvermatrix = getinvermatrix)
}


## This function first verified in the function above if the inverse matrix
#has been calculated, if it has been gives the message "getting inverse matrix from cached"
#and then shows the inverse matrix, if it has not been calculated  it
# gets the original matrix from the function above and computes the inverse 
#matrix, storing the results in the function makeCachematrix and finally returns
#the inverse matrix. 

cacheSolve <- function(x, ...) {
        inverse <- x$getinvermatrix()
        if(!is.null(inverse)) {
                message("getting inverse matrix from cached")
                return(inverse)
        }
        matrix <- x$getmatrix()
        inverse <- solve(matrix, ...)
        x$setinvermatrix(inverse)
        inverse
}


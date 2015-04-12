#########################################################################################################################
##                                                                                                                     ##
## We write the following two functions:                                                                               ##
##                                                                                                                     ##
##  1. makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.                    ##
##  2. cacheSolve:      This function computes the inverse of the special "matrix" returned by makeCacheMatrix above,  ##
##                      retrieving the inverse from the cache if it has already been calculated.                       ##
##                      cacheSolve assumes the matrix supplied to be invertible.                                       ##
##                                                                                                                     ##
#########################################################################################################################

## The makeCacheMatrix function creates a special "matrix", which is really a list containing a function to
##  1. set the value of the matrix
##  2. get the value of the matrix
##  3. set the value of the matrix inverse
##  4. get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
		# initialization
        matrixInverse <- NULL
        # set the value of the matrix
        set <- function(y) {
                x <<- y
                matrixInverse <<- NULL
        }
        # get the value of the matrix
        get <- function() x
        # set the value of the matrix inverse
        setInverse <- function(inverse) matrixInverse <<- inverse
        # get the value of the matrix inverse
        getInverse <- function() matrixInverse
        # define the list of these 4 functions
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## The cacheSolve function calculates the inverse of the special "matrix" created with the above function.
## However, it first checks to see if the matrix inverse has already been calculated. If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the matrix and stores such an inverse in the cache via the setInverse function.
## As an assumption, the matrix supplied has to be invertible.

cacheSolve <- function(x, ...) {
		# First tries to get the cached value of the matrix inverse of 'x': if it exists, the function directly returns its value
        matrixInverse <- x$getInverse()
        if(!is.null(matrixInverse)) {
                message("getting cached inverse")
                return(matrixInverse)
        }
        # If a cached inverse does not exist, the function gets the value of 'x' and calculates its inverse with the solve function
        data <- x$get()
        matrixInverse <- solve(data, ...)
        # Stores the matrix inverse after calculation
        x$setInverse(matrixInverse)
        ## Returns a matrix that is the inverse of 'x'
        matrixInverse
}

## Two simple test examples to show that cacheSolve+makeCacheMatrix works and I can get matrix inverses:
## 
## testMatrix1 <- matrix(c(2,0,0,2),2,2)
## cacheSolve(makeCacheMatrix(testMatrix1))
##
## testMatrix2 <- matrix(c(1,2,3,4,3,2,1,2,3,4,3,2,0,1,2,3),4,4)
## cacheSolve(makeCacheMatrix(testMatrix2))
##
##
## Much more useful example to test the "getting cached inverse" component
## I generate a 2000x2000 matrix made of random numbers (following a normal distribution with mean 2 and standard deviation 1),
## since the probability that such a matrix is invertible is very high.
## This matrix is 30.5MB.
## 
## matrix1 <- matrix(rnorm(4000000,2,1),2000,2000)
## 
## Comparison of processing time in my PC (R v.3.1.3 64bit on Windows 7 - intel i5, 2.7GHz)
## 1. Calculation with solve function
## system.time(solve(matrix1))
##    user  system elapsed 
##    7.85    0.00    7.85
## 
## Same matrix, but defined with makeCacheMatrix
## matrix2 <- makeCacheMatrix(matrix1)
## 2. First execution of cacheSolve: actual calculation of matrix inverse again with solve function
## system.time(cacheSolve(matrix2))
##    user  system elapsed 
##    7.80    0.03    7.83
## 3. Second execution of cacheSolve: skip solve getting cached data ---> of course very fast
## system.time(cacheSolve(matrix2))
## getting cached data
##    user  system elapsed 
##       0       0       0
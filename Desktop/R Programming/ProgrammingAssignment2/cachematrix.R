## makeCacheMatrix will store value of input matrix and create functions
## to manipulate the matrix
## cacheSolve will return inverse of matrix if it has been stored or
## calculate the inverse using functions created by makeCacheMatrix function

## this function receives as input a square matrix
## and returns a list containing 4 functions:
## 1. set - function setting x to a matrix y from *all* environments
##      (not just current one) and setting m to NULL in *all* environments
## 2. get - empty function returning value of input matrix
## 3. setsolve - takes an input inverse matrix and sets it to m from
##      all environments
## 4. getsolve - empty function returning value of output inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
        
}


## this function
## 1. checks to see if input object already has a solved inverse matrix m
## 2. returns that matrix m if it exists
## 3. takes input matrix from input object and calculates inverse matrix m
## 4. sets value of inverse matrix in all environments
## 5. returns calculated value of inverse matrix m

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}

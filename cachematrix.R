## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# makeCacheMatrix creates a function where it creates an x value and then sets a y value to the x value. Once that is done, the value is set to a variable.
makeCacheMatrix <- function(x = matrix()) {
            m <- NULL
            set <- function(y) {       #setting the value of matrix and then setting it to the value of x
                    x <<- y            # inverse calculation
                    m <<- NULL
            }
            get <- function() x        # getting the value of matrix
            setinverse <- function(inverse) m <<- inverse       #setting the inverse of matrix
            getinverse <- function() m                    #getting the inverse of matrix
            list(set = set, get = get,setinverse = setinverse, getinverse = getinverse)                    # list takes the values of variables
}


## Write a short comment describing this function
#The cacheSolve returns the inverse value if the value is not null. If it is, the inverse will be calculated, set and returned.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
            m <- x$getinverse()            # m is assigned to the calculated inverse from the above function
            if(!is.null(m)) {              # if there is an inverse value 
                    message("getting cached data")
                    return(m)
            }
            data <- x$get()                # if there isn't an inverse value it will be solved
            m <- solve(data, ...)          # solve function calculates the inverse and assigns to m
            x$setinverse(m)        
            m

}

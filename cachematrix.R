## Setting a square matrix x and value y which is the input vector. Setting inverse of the matrix
## to be null and then solving its cache through the solve command.

makeCacheMatrix <- function(x = matrix()){      ## Defining square matix.
        i <- NULL                               ## This is our inverse
        set <- function(y){                     ## The input vector
                x <<- y                         ## Saving the input vector as x
                i <<- NULL                      ## Restting inverse to NULL
        }
        get <- function() x                             ## Return the original value of x
        setInverse <- function(solve) i <<- inverse     ## Assgning cacheinverse (i)  
        getInverse <- function() i
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Function for returning the cached value of (x).
## If inverse(i) is not null, pprint message "getting cached message" and return (i).

cacheSolve <- function(x, ...) {                                ## Input already created by makeCacheMatrix
        i <<- x$getInverse()                        ## Returns the value of inverse though accessing x
        if(!is.null(i)){                            ## Condition stating if i is not NULL
                message("getting cached inverse")   ## For i is not NULL, get message
                return(i)                           ## return inverse
        }
        data <- x$get()
        i <- solve(data,...)
        x$setInverse(i)                         ## Store the inverse in x that is cached everytime x is accessed.
        i                                       ## Return a matrix that is the inverse of 'x'
}
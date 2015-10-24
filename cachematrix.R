## ASSIGNMENT:  Caching the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x=matrix()){
    
    inverse_matrix <- NULL
    
    get <- function() x                        # Get matrix
    getinverse <-function() inverse_matrix     # Get inverse of the Matrix
    set <-function(y){                         # Set matrix
        x <<- y
        inverse_matrix <<- NULL
    }
    setinverse <- function(inv){               # Set inverse of the matrix
        inverse_matrix <<- inv
    }
    list(get=get, set=set, getinverse=getinverse, setinverse=setinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
cacheSolve <- function(x, ...){
    
    inverse_matrix <- x$getinverse()
    if(!is.null(inverse_matrix))             # If the inverse has already been calculated,
    {                                        # then retrieve inverse from cache
        message("getting cached data")
        return(inverse_matrix)
    }
   
    data <- x$get()
    inverse_matrix <- solve(data)            # Calculating inverse with solve()
    x$setinverse(inverse_matrix)
    inverse_matrix
    
}

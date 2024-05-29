## makeCacheMatrix: This function creates a special "matrix" object
## that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL  # Initialize the inverse property to NULL
    
    set <- function(y) {  # Define a function to set the matrix
        x <<- y  # Assign the input matrix 'y' to the variable 'x' in the parent environment
        inv <<- NULL  # Reset the inverse property because the matrix has changed
    }
    
    get <- function() x  # Define a function to get the matrix 'x'
    
    setInverse <- function(inverse) inv <<- inverse  # Define a function to set the inverse property
    
    getInverse <- function() inv  # Define a function to get the inverse property
    
    list(set = set, get = get,  # Return a list of the above functions so they can be accessed
         setInverse = setInverse,  # Function to set the inverse
         getInverse = getInverse)  # Function to get the inverse
}

## cacheSolve: This function computes the inverse of the special
## "matrix" returned by makeCacheMatrix above. If the inverse has
## already been calculated (and the matrix has not changed), then
## cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    inv <- x$getInverse()  # Attempt to get the cached inverse
    
    if (!is.null(inv)) {  # Check if the cached inverse is not NULL
        message("getting cached data")  # Print a message indicating that cached data is being used
        return(inv)  # Return the cached inverse
    }
    
    data <- x$get()  # Get the matrix from the special "matrix" object
    
    inv <- solve(data, ...)  # Compute the inverse of the matrix
    
    x$setInverse(inv)  # Cache the computed inverse in the special "matrix" object
    
    inv  # Return the computed inverse
}

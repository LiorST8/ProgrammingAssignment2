## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
# Initialize an object to store the cached inverse
        inv <- NULL

# Method to set the matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL # Reset inverse cache when the matrix is updated
}

# Method to get the matrix
        get <- function() x

# Method to set the inverse
        Set_Inverse <- function(inverse) inv <<- inverse

# Method to get the inverse
        Get_Inverse <- function() inv

# Return a list of the methods
        list(set = set, get = get,
             Set_Inverse = Set_Inverse,
             Get_Inverse = Get_Inverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
# Retrtieve the cached inverse
        inv <- x$Get_Inverse()

# If the inverse is already cached then return it
        if(!is.null(inv)) {
                message("Getting cached data")
                return(inv)      
}

# Otherwise, calculate the inverse
        data <- x$get()
        inv <- solve(data, ...)

# Cache the inverse for future use
        x$Set_Inverse(inv)

inv
}

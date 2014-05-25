## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(mx = matrix()) {
        inv_mx <- NULL
        set.mx <- function(y) {
                mx <<- y
                inv_mx <<- NULL
        }
        get.mx <- function() mx
        set.inv_mx <- function(mi) inv_mx <<- mi
        get.inv_mx <- function() inv_mx
        list(set.mx = set.mx, get.mx = get.mx,
             set.inv_mx = set.inv_mx,
             get.inv_mx = get.inv_mx)

}


## Write a short comment describing this function

cacheSolve <- function(mx, ...) {
        
        inv_mx <- mx$get.inv_mx()
        if(!is.null(inv_mx)) {
                message("getting cached data")
                return(inv_mx)
        }
        data <- mx$get.mx()
        inv_mx <- solve(data, ...)
        mx$set.inv_mx(inv_mx)
        inv_mx
        
        
        ## Return a matrix that is the inverse of 'x'
}

## Put comments here that give an overall description of what your
## functions do

## Stores a matrix in cache


makeCacheMatrix <- function(mx = matrix()) {
        inv_mx <- NULL #matrix initilization
        set.mx <- function(y) { #stores the original matrix in variable "y"
                mx <<- y
                inv_mx <<- NULL
        }
        get.mx <- function() mx  #retrieves the original matrix
        set.inv_mx <- function(mi) inv_mx <<- mi
        get.inv_mx <- function() inv_mx
        list(set.mx = set.mx, get.mx = get.mx,
             set.inv_mx = set.inv_mx,
             get.inv_mx = get.inv_mx)

}


## get the inverse of the matrix stored in cahe

cacheSolve <- function(mx, ...) {
        
        inv_mx <- mx$get.inv_mx() #get the matrix stored in cahce
        if(!is.null(inv_mx)) {          # determines if the matrix is in the cache
                message("getting cached data")
                return(inv_mx)
        }
        data <- mx$get.mx()     #get the original matrix
        inv_mx <- solve(data, ...)      #get the inverse of the original matrix
        mx$set.inv_mx(inv_mx)
        inv_mx
        
        
        ## Return a matrix that is the inverse of 'mx'
}

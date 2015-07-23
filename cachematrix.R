## The functions in this file calculate the inverse of the supplied matrix. However, in order to avoid the repetition of compute
## intensive operation on an unchanged matrix, the matrix and its inverse will be calculated the first time and cached using <<-
## operator. This way, the subsequent calls to compute the matrix will result in retrieving the inverse from cache and not 
## recompute it.


## This function creates a special "matrix" object that can cache its inverse. It returns a list of functions to set and get the 
## original matrix objects and its inverse. It uses <<- assignment operator to cache the objects in a different environment.
makeCacheMatrix <- function(x = matrix()) {
        ## Declare the matrix inverse variable
        matinv <- NULL
        
        ## Declare the set function to store the matrix and initialize matrix inverse variable into cache
        set <- function(y){
                x <<- y
                matinv <<- NULL
        }
                
        ## Declare the get function to return the original matrix
        get <- function(){
                x
        }
        
        ## Declare the setmatinv function that stores the given matrix inverse object into the cache variable
        setmatinv <- function(inv){
                matinv <<- inv
        }
        
        ## Declare the getmatinv function that simply returns the cached matrix inverse object
        getmatinv <- function(){
                matinv
        }
        
        ## Finally, return a list containing the above defined functions.
        list(set = set,
             get = get,
             setmatinv = setmatinv,
             getmatinv = getmatinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## Check if the inverse object is already computed and cached and if it is then return
        inv <- x$getmatinv()
        
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        
        ## Inverse Matrix was not cached, so compute the inverse, cache it and return
        mat <- x$get()
        inv <- solve(mat)
        
        ## Store it in cache
        x$setmatinv(inv)
        
        ## Return inverse object
        inv
}


# To test
# a <- matrix(1:4, nrow = 2, ncol = 2)
# ca <- makeCacheMatrix(a)
# 
# > cacheSolve(ca)
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > cacheSolve(ca)
# getting cached data
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5

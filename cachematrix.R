# ## Put comments here that give an overall description of what your
# ## functions do
# 
# ## implements get , set, setinverse and getinverse functions
# ## get -> to get the original matrix
# ## set -> a wrapper over the original matrix with the inverse set as NULL
# ## getinverse -> returns the cached inverse
# ## setinverse -> sets the inverse to be cached
# 
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        
        get <- function() x
        
        setinverse <- function(solve) inv <<- solve
       
        getinverse <- function() inv
        
        ## Special matrix with the collection of special functions
        matrix (data=c (set,get,setinverse,getinverse),dimnames= list(c("set","get","setinverse","getinverse"),c("func"))
        )
        
        
}

 
## Function which takes a special matrix as argument and returns the cached inverse of the matrix
## or calculates the inverse and caches it in the special matrix and then returns it

cacheSolve <- function(x, ...) {
        
        
        ## Tries to get the cached inverse from the special matrix
        inv <- (x[["getinverse","func"]])()  
        
        ## Not Null means return the inverse obtained from the cache
        if(!is.null(inv)){
                message("getting cached inverse")
                return (inv)
        }
        
        ## You got here because there was nothing in the cache. First time !!
        data <-x[["get","func"]]() ## gets the original matrix
        
        inv <- solve(data,...) ## calculates the inverse
        
        x[["setinverse","func"]](inv) ## Sets the inverse in the cache of the special matrix
        
        inv  ## returns the inverse
}

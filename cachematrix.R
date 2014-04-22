## This 1st function creates an "special matrix" which 
## is a list containing functions to: 
##1) set the value of the matrix (set function)
##2) get the value of the matrix (get function)
##3) set the value of the inverse matrix (setinv function)
##4) get the value of the inverse matrix (getinv function)

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y){
		x <<- y
		inv <<-NULL
	}
	get <- function() x
	setinv <- function(inverse) inv <<- inverse
	getinv <- function() inv 
	list(set = set, get = get, 
		setinv = setinv, getinv = getinv)
}

## This second function calculates the inverse of the 
##special matrix created before. If the inverse is already
##calculated and is in the cache it "caches" the matrix 
##rather than compute it again. 

cacheSolve <- function(x, ...) {
	inv <- x$getinv()                          ## queries the cache for inv
		if(!is.null(inv)) {					   ## if is in the cache, return inv
				message("getting cached data") 
				return(inv)
		}	                                   ## if is not in the cache,
		data <- x$get()                        ## get the matrix 
		inv <- solve(data,...)                 ## compute the inverse 
		x$setinv(inv)                          ## save the result in x´s cache.  
		inv        
}

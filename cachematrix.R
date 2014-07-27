## https://class.coursera.org/rprog-005/forum/thread?thread_id=139

## makeCacheMatrix creates a list of functions:
## 	set(cachematrix) - set the value of the matrix
## 	get() - get the value of the matrix
## 	setinverse(inverse) the value of the mean
## 	getinverse() the value of the mean

makeCacheMatrix <- function(cachematrix = matrix()) {

	#Initialize the inverted matrix
    inv <- NULL
	
	#Store the matrix in the parent enviornment
    set <- function(tempmatrix) {
        cachematrix <<- tempmatrix
        inv <<- NULL
    }
	
	#Return the value stored in the cached matrix
    get <- function() cachematrix
	
	#Store the inverted matrix in the parent enviornment
    setinverse <- function(inverse) inv <<- inverse
	
	#Return the inverted matrix stored in the parent enviornment
    getinverse <- function() inv
	
	#Return a list object with the four functions
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## cacheSolve returns the inverted matrix from cache if it has already been calculated. Otherwise is calculates the inverse of the matrix and calls the setinverse function to set the value in cache.

cacheSolve <- function(x, ...) {

	#Call the getinverse function of the makeCacheMatrix object
    inv <- x$getinverse()
	
	#If an inverse matrix is stored in inv, return it 
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
	
	#Otherwise get the input matrix
    data <- x$get()
	
	#Calculate the inverse of the matrix
    message("computing and caching inverse.")
	inv <- solve(data)
    
	#Store the inverted matrix 
	x$setinverse(inv)
    
	#Return the inverted matrix
	inv
}

## Sample Run
## 
## Create a matrix and call makeCacheMatrix
##> x = rbind(c(3, -4), c(2, -5))
##> m = makeCacheMatrix(x)
## 
## Get the input matrix
##> m$get()
##     [,1] [,2]
##[1,]    3   -4
##[2,]    2   -5
## 
## Get the inverted matrix. Must compute it because it's not cached.
##> cacheSolve(m)
##computing and caching inverse.
##          [,1]       [,2]
##[1,] 0.7142857 -0.5714286
##[2,] 0.2857143 -0.4285714
## 
## Get the inverted matrix again, this time from cache.
##> cacheSolve(m)
##getting cached data.
##          [,1]       [,2]
##[1,] 0.7142857 -0.5714286
##[2,] 0.2857143 -0.4285714
## R programming Assignment 2
## Agust 2017
## authur : 02288252525
## The code is make for Caching the Inverse of a Matrix by using two function
## makeCaheMatirx and cacheSolve

# makeCacheMatrix, create a special list contain 4 function, which can cache the 
# inverse of matrix, briefly th four functions is
# 1.set the value of matrix
# 2.get the value of matirx
# 3.set the value of matrix inversion
# 4.get the value of matrix inversion

makeCacheMatrix <- function(x=matrix()){
        i <- NULLset <- function(y){
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
        
}

# cacheSolve,  it compute the inverse of matrix from the input of makeCacheMatrix
# and print out the value of inverse matrix
# If the inverse has already calculate it retrieve the inverse from the cache
# and print out the message "getting cached data"

cacheSolve <- function(x,...){
        i <- x$getinverse()
        if(!is.null(i)){
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data,...)
        x$setinverse(i)
        i
}
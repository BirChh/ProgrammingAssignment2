## There are two functions MakeCacheMatrix and CacheSolve
## The first function consists of set, get, setinve and getinv used to get the input matrix
##and the second function  get cache data and computes the inverse 
        
#add library MASS which is used to calculate inverse of non-square matrix
library(MASS)

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL ##initialise NULL for inverse matrix of x
        set <- function(y){
                x<<-y
                inv<<-NULL
        }
        get <- function()x #function to get matrix x
        setinv <- function(inverse)inv<<-inverse
        getinv <- function(){
                inver <- ginv(x) #function to get inverse of the matrix
                inver%*%x 
        }
        list(set = set, get = get, setinv = setinv, getinv = getinv)

}


## this function is to get the cahce data

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.na(inv)){            #checking if inverse is null
                message("getting cache data!") 
                return(inv)    # returns inverse value
        }
        data <- x$get()
        inv <- solve(data,...) #calculate inverse values for the inverse matrix of x
        x$setinv(inv)
        inv 
}

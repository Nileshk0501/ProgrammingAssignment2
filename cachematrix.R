## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

#@x: A matrix
#working: The function performs
        #get the matrix
        #set the matrix
        #get the Inverse of matrix
        #set the Inverse of the matrix
        #return a lsit that will be used as input to cacheSolve
        INV <- NULL
        set <- function(y) {
                      # '<<-' used to assign a value to an object in an environment
                      # other than this environment
                      x<<-y
                      INV <<- NULL
                }
         get <- function() x
         setINV <- function() INV <<- I
         getINV <- function() INV
         list(set = set, get = get, setINV = setINV, getINV = getINV)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
       ## Return a matrix that is the inverse of 'x'
       #@x: output of makeCacheMatrix
       #return: inverse of matrix
       INV <- x$getINV()
       # If the inverse is already calculated

       if(is.na(INV)){
                   # get it from the cacheMatrix() and skip the computation of inverse
                   message("Getting cahched data")
                   return(INV)
          }else{
                    mat.data <- x$get
                    INV <- solve(mat.data,...)

                    #set the Inverse of matrix in the cache
                    x$setINV(INV)
                    
                    #return the inverse of matrix as output
                    return(INV)
               }
 }


makeCacheMatrix <- function(x){
        matVal <- NULL
        setVal <- function(y){
                x <<- y
                matVal <<- NULL
        }
        getVal <- function() x
        setMat <- function(matSolve) matVal <<- matSolve
        getMat <- function() matVal
        list(set = setVal, get = getVal, setSolved = setMat, getSolved = getMat)
}

cacheSolve <- function(x, ...){
        matVal <- x$getSolved()
        if(!is.null(matVal)){
                message("Getting cached data")
                return (matVal)
        }
        dataMat <- x$get()
        matVal <- solve(dataMat)
        x$setSolved(matVal)
}
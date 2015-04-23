##Script to evaluate the inverse matrix as well as cache the inverse matrix
##so that it can be retrived as required if no changes are made to the supplied
##matrix

makeCacheMatrix <- function(x){
        matVal <- NULL
        
        setVal <- function(y){   #This function assigns new matrix to 'x'     
                x <<- y
                
                matVal <<- NULL  #The value of should be reverted back to NULL
        }
        getVal <- function() x   #Returns the matrix set by setVal
        
        setMat <- function(matSolve) matVal <<- matSolve     #Sets the inverse matrix    
        
        getMat <- function() matVal     #Returns the desired inverse matrix      
        
        #Formation of list that contains functions 
        list(set = setVal, get = getVal, setSolved = setMat, getSolved = getMat)
}

#Inverse matrix to be supplied by this function below
cacheSolve <- function(x, ...){
        matVal <- x$getSolved()         #Sets the matVal with previous inverse matrix
        
        if(!is.null(matVal)){           #Condition for checking if previous result was NULL or not
                message("Getting cached data")
                
                return (matVal)         #if not then returns the cached inverse matrix
        }
        dataMat <- x$get()              #if yes then sets the dataMat variable with new matrix
        
        matVal <- solve(dataMat)        #Special function 'Solve' to retrieve inverse matrix of supplied matrix
        
        x$setSolved(matVal)             #Sets the function with a inverse matrix 
}
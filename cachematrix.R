##-----------------------------------------------------
## Programming Assignment2
## Assignment: Caching the Inverse of a Matrix
##-----------------------------------------------------
## [Instructions]
## Matrix inversion is usually a costly computation and there may be some benefit to caching 
## the inverse of a matrix rather than compute it repeatedly (there are also alternatives 
## to matrix inversion that we will not discuss here). 
## Your assignment is to write a pair of functions that cache the inverse of a matrix.
## 
## Write the following functions:
## 1. makeCacheMatrix()
##    : This function creates a special "matrix" object that can cache its inverse.
## 2. cacheSolve()
##    : This function computes the inverse of the special "matrix" returned
##      by makeCacheMatrix above. 
##      If the inverse has already been calculated (and the matrix has not changed),
##      then the cachesolve should retrieve the inverse from the cache.
## 
## Computing the inverse of a square matrix can be done with the solve function in R.
## For example, if X is a square invertible matrix, then solve(X) returns its inverse.
############################
## 1. makeCacheMatrix()
## : This function creates a special "matrix" object that can cache its inverse.
############################
makeCacheMatrix <- function(x = matrix()) {
    inverse_matrix <- NULL
    
    ## set x matrix
    set <- function(y) {
        x <<- y
        inverse_matrix <<- NULL
    }

    ## get x matrix
    get <- function() x
    
    ## set inverse matrix
    setInverseMatrix <- function(solve) {
        inverse_matrix <<- solve
    }
    
    ## return inverse matrix
    getInverseMatrix <- function() inverse_matrix
    
    ## return variable
    return_vector <- list(set=set
                          , get=get
                          , setInverseMatrix=setInverseMatrix
                          , getInverseMatrix=getInverseMatrix
                          )
    
    return(return_vector)
}

############################
## 2. cacheSolve()
## : This function computes the inverse of the special "matrix" returned
## by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.
## 
## Computing the inverse of a square matrix can be done with the solve function in R.
## For example, if X is a square invertible matrix, then solve(X) returns its inverse.
############################
cacheSolve <- function(x, ...) {
    ## get inverse matrix
    inverse_matrix <- x$getInverseMatrix()
    
    ## null check
    if (!is.null(inverse_matrix)) {
        message('Get data from the cache!!!')
        return(inverse_matrix)
    } else {
        data <- x$get()
        inverse_matrix <- solve(data, ...)
        x$setInverseMatrix(inverse_matrix)
    }

    ## Return a matrix that is the inverse of 'x' matrix
    return(inverse_matrix)
}


############################
## Call function to create a inverse matrix
############################
## 1st square matrix
x <- as.matrix(cbind(c(1, 0, -1)
                     , c(1, 2, 1)
                     , c(3, 4, 0)
                ))
class(x)
# [1] "matrix" "array"

## get determinant of a square matrix 
det(x)  
# [1] -2
 
solve(x)

## call makeCacheMatrix()
inv_mat <- makeCacheMatrix(x=x)

inv_mat

cacheSolve(inv_mat)
# [,1] [,2] [,3]
# [1,]    2 -1.5    1
# [2,]    2 -1.5    2
# [3,]   -1  1.0   -1

cacheSolve(inv_mat)
# Get data from the cache!!!
#     [,1] [,2] [,3]
# [1,]    2 -1.5    1
# [2,]    2 -1.5    2
# [3,]   -1  1.0   -1

cacheSolve(inv_mat)
# Get data from the cache!!!
#     [,1] [,2] [,3]
# [1,]    2 -1.5    1
# [2,]    2 -1.5    2
# [3,]   -1  1.0   -1

## 2nd square matrix
x2 <- as.matrix(cbind(c(1, 1, 0)
                     , c(0, 1, 1)
                     , c(1, 1, 1)
))

det(x2)
# [1] 1

solve(x2)
ls()

## change x matrix
inv_mat$set(x2)

cacheSolve(inv_mat)
# [,1] [,2] [,3]
# [1,]    0    1   -1
# [2,]   -1    1    0
# [3,]    1   -1    1

cacheSolve(inv_mat)
# Get data from the cache!!!
#     [,1] [,2] [,3]
# [1,]    0    1   -1
# [2,]   -1    1    0
# [3,]    1   -1    1

cacheSolve(inv_mat)
# Get data from the cache!!!
#     [,1] [,2] [,3]
# [1,]    0    1   -1
# [2,]   -1    1    0
# [3,]    1   -1    1

inv_mat2 <- makeCacheMatrix(x2)

cacheSolve(inv_mat2)

cacheSolve(inv_mat2)

cacheSolve(inv_mat2)


##------- The end of Assignment: Caching the Inverse of a Matrix ------------


##-------- [Example] --------------------------------------------------------
## Put comments here that give an overall description of what your functions do.
############################
## [Example: Caching the Mean of a Vector]
##        - used two functions, makeVector() and cachemean().
## Example 1. makeVector()
## - creates a special "vector", which is really a list containing a function to
## a. set the value of the vector
## b. get the value of the vector
## c. set the value of the mean
## d. get the value of the mean
############################
makeVector <- function(x = numeric()) {
    m <- NULL
    
    ## set()    
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    ## get()
    get <- function() x
    
    ## set mean
    setmean <- function(mean) m <<- mean
    
    ## get mean
    getmean <- function() m
    
    ## return value
    ret <- list(set = set
                , get = get
                , setmean = setmean
                , getmean = getmean)
    
    return(ret)
}

############################
## Example 2. cachemean()
## - calculates the mean of the special "vector" created with the above function.
## However, it first checks to see if the mean has already been calculated. 
## If so, it gets the mean from the cache and skips the computation. 
## Otherwise, it calculates the mean of the data and sets the value of the mean in the cache
## via the setmean function.
############################
cachemean <- function(x, ...) {
    ## get mean
    m <- x$getmean()
    
    ## check null for mean of x
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    ## get data
    data <- x$get()
    
    ## calculate mean
    m <- mean(data, ...)
    
    ## set means
    x$setmean(m)
    
    return(m)
}

############################
## Call Example Functions
############################
x <- 1:10
vec <- makeVector(x=x)

cachemean(x=vec)
# [1] 5.5

cachemean(vec)
# getting cached data
# [1] 5.5

## set x
vec$set(y=1:5)

cachemean(vec)
# [1] 3

cachemean(vec)
# getting cached data
# [1] 3

##------------ Example: Caching the Mean of a Vector ------------------------

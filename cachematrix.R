# Programming Assignment 2: Lexical Scoping
# reference: https://class.coursera.org/rprog-007/human_grading/view/courses/972580/assessments/3/submissions

# run the test codes 
# > m<-makeCacheMatrix()
# > m$set(matrix(c(4,2,7,6), 2,2))
# > cacheSolve(m)
# calculating matrix inverse cached data
# [,1] [,2]
# [1,]  0.6 -0.7
# [2,] -0.2  0.4

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached matrix inverse")
        return(i)
    }
    data <- x$get()
    message("calculating matrix inverse")
    i <- solve(data, ...)
    x$setinverse(i)
    i
}

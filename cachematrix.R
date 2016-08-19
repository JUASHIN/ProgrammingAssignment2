makeCacheMatrix <- function(x = matrix()){
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) m <<- inverse
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

cacheSolve <- function(x, ...){
        m <- x$getInverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}


> source("mycachematrix.R")
> ls()
[1] "cacheSolve"      "makeCacheMatrix"
> fun_cachematrix <- makeCacheMatrix(matrix(1:4, 2, 2))
> fun_cachematrix$get()
     [,1] [,2]
[1,]    1    3
[2,]    2    4
> cacheSolve(fun_cachematrix)
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
> cacheSolve(fun_cachematrix)
getting cached data
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
> fun_cachematrix$getInverse()
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5

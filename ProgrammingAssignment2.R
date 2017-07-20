#The makeCacheMatrix() function stores the matrix in the cache.. The set function, get function, setinverse and getinverse functions are very similar to the mean example which we had. I have changed the definition of x variable from numeric to matrix. The set is used for setting the matrix to variable x. The get is used for retrieving the value. The setinverse is used to store the inverse value in the cache and getinverse is used to retrieve the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

#The function cacheSolve is used for finding the inverse of a matrix. The solve() function has an argument as the matrix and the result, which is the inverse of the matrix is stored in m. The is.null(m) checks wether the inverse value is null, which means there is no data which is cached and therefore we use the solve function and get the result and store in the cached memory using the x$setinverse() function. If the cache already contains a value we print the "getting cached data" statement and return the value stored in the cached memory using the return() function.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
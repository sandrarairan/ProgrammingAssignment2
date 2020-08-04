## funciones que almacenan el caché inverso de una matriz

## makeCacheMatrix: esta función crea un objeto "matriz"
## que puede almacenar el caché su inverso

makeCacheMatrix <- function(x = matrix()) {
        inversa <- NULL
        set <- function(y) {
                x <<- y
                inversa <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inversa <<- inverse
        getinverse <- function() inversa
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## calcula el inverso de la "matriz"  devuelta por la funcion makeCacheMatrix. Si ya se ha calculado el inverso, entonces el caché debe recuperar el inverso del caché.
## solve(X) returns its inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inversa <- x$getinverse()
        if(!is.null(inversa)) {
                message("getting cached data")
                return(inversa)
        }
        data <- x$get()
        inversa <- solve(data, ...)
        x$setinverse(inversa)
        inversa
}

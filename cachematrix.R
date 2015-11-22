## The following functions work together to create a square invertible matrix
## and make the inverse of the matrix available in the cache environment


## makeCacheMatrix creates and returns a list of functions
## used by cacheSolve to get or set the inverted matrix in cache.
makeCacheMatrix <- function(x = matrix()) {
    m<-NULL
    set<-function(y){
        x<<-y
        m<<-NULL
    }
    get<-function() x
    setmatrix<-function(solve) m<<- solve
    getmatrix<-function() m
    list(set=set, get=get,
         setmatrix=setmatrix,
         getmatrix=getmatrix)
}


## cacheSolve calculates the inverse of the matrix created in makeCacheMatrix
## If the inverted matrix does not exist in cache,
## it is created in the working environment and it's inverted value
## is stored in cache.
cacheSolve <- function(x=matrix(), ...) {
    m<-x$getmatrix()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    matrix<-x$get()
    m<-solve(matrix, ...)
    x$setmatrix(m)
    m
}

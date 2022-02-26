## create a special matrix
makeCacheMatrix<-function(x=matrix()){
    i<-NULL
    set<-function(y){
        x<<-y
        m<<-NULL
    }
    get<-function()x
    ## get the matrix
    setinverse<-function(solve)i<<-solve
    getinverse<-function()i
    ## get the inverse of the matrix
    list(set=set,get=get,
         setinverse=setinverse,
         getinverse=setinverse)
}
## compute the inverse of the special matrix 
## returned by the function makeCacheMatrix
cacheSolve<-function(x,...){
    i<-x$getinverse()
    if (!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data<-x$get()
    i<-solve(data,...)
    x$setinverse(i)
    i
    ## Return a matrix that is the inverse of 'x'
}
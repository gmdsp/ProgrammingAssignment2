## Functions makeCacheMatrix and cacheSolve enable more efficient matrix inverse computations 
## by avoiding unnecessary matrix inverse computations if matrix inverse has already been computed
## and there is no change in the matrix whose inverse has to be computed. This can greatly improve
## speed of programs that use loops and compute matrix inverses inside the loops.




## The makeCacheMatrix function takes a matrix as an argument and creates a list of functions
## that can set the matrix, get the matrix, set the matrix's inverse and get the matrix's inverse

makeCacheMatrix <- function(x = matrix()) {
    inv<-NULL  ## Matrix inverse set to null
    
    set<-function(y=matrix()) {    ## set function assingn new matrix to a variable
        x<<-y       ## assign the set function's argument (y) to the variable x from the parent environment
        inv<<-NULL  ## matrix inverse not computed, so it is set to NULL
    }
    get<-function() x   ## get function reads the matrix 
    getInv<-function() inv   ## getInv function reads the matrix inverse
    setInv<-function(m_inv) inv<<-m_inv   ## setInv function assigns the matrix inverse to the variable inv from
                                          ## the pharent environment
    list(set = set,get = get, getInv=getInv, setInv=setInv)   ## makeCacheMatrix function returns the list of functions
}


## The cacheSolve function checks if matrix, formated as a list by the function makeCacheMatrix, 
## passed as an argument has its inverse already computed. If not it computes the 
## matrix inverse. 
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv<-x$getInv()   ## read matrix inverse   
    if(!is.null(inv))   ## check if matrix inverse has already been computed
    {
        message("getting cached matrix inverse")
        return(inv)     ## if the matrix inverse has been computed, read it from cache
    }
    else   ## if matrix inverse has not been already computed, compute it using solve() function
    {
        m_x<-x$get()
        inv<-solve(m_x,...)   ## compute matrix inverse
        x$setInv(inv)
        return(inv)
    }
}

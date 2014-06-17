#assume the matrix supplied is always invertible and square

#Here are two functions that create a cache of the inverse of a matrix called
#makeCacheMatrix and make a second function that retreives the inverse or 
#computes it called cacheSolve

#create a function that takes a matrix and computes the inverse and then stores it
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL                                #create and initialize vector
        set <- function(y) {
                x <<- y
                m <<- NULL
        }                                          #"set" initializes makeVector so user can input vector
        get <- function() x                        #"get" returns the user's vector
        setinv <- function(solve) m <<- solve     #calculate the inverse of the local matrix
        getinv <- function()                 
                
                #now return an object that has functions set, get, setinv and getinv
                list(set = set, get = get,
                     setinv = setinv,
                     getinv = getinv)
}

#return the inverse of a cached matrix or calculate the inverse if not already cached
cachSolve <- function(x, ...) {
        m <- x$getinv()                         #store the contents of the getinv function into m
        if(!is.null(m)){                        #if m is not empty,
                message("getting cached value") #return this message AND
                return(m)                       #return the local inverse cached previously
        }
        data <- x$get()                         #if there is no cached inverse
        m <- solve(data, ...)                   #calculate the mean
        x$setinv(m)                             #store it in the setinv
        m                                       #return the inverse
}

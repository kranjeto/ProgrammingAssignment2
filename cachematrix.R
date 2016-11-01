## returns matrix object (matrix class with get/set methods)

makeCacheMatrix <- function(x = matrix()) {
        # matrix inverse value
        xs <- NULL
        
        # get matrix value
        get <- function(){
                x
        }

        # set matrix value (and clear cache)
        set <- function(mat){
                x <<- mat
                xs <<- NULL
        }

        # get matrix inverse
        getInverse <- function(){
                xs
        }
        
        # set matrix inverse
        setInverse <- function(s){
                xs <<- s
        }
        
        list(
                get = get,
                set = set,
                getInverse = getInverse,
                setInverse = setInverse
        )
}


## returns inverse of matrix 

cacheSolve <- function(x, ...) {
        mi <- x$getInverse()

        # check if value is cached
        if(!is.null(mi)){
                message("Cache hit!")
                return (mi)
        }else{
                message("Calculating inverse...")
                m <- x$get()
                mi <- solve(m)
                x$setInverse(mi)       
        }
        mi
}


## usage

# create matrix
m <- matrix(runif(100,0,100),c(10,10))
cm <- makeCacheMatrix(m)

# get inverse (calculate)
mi <- cacheSolve(cm)

# get inverse (cache hit)
mi2 <- cacheSolve(cm)

# compare results
identical(mi,mi2)

# check calculation of inverse
round(m%*%mi)

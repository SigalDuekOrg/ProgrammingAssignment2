#################################################
#                                               #
#            Introduction to R                  #
#               Assignment 2                    #
#                                               #
#            Student: Sigal Duek                #
#                                               #
#################################################

# A pair of functions that cache the inverse of a matrix.

###################################################
# This function creates a special "matrix" object #  
# that can cache its inverse.                     #
###################################################
makeCacheMatrix <- function(mat = matrix()) {
  inv <- NULL
  set <- function(y) {
    if (!is.matrix(y)){
      stop("error - input is not a matrix")
      
    }
    mat <<- y   
    inv <<- NULL    
  }
  
  get <- function() mat
  
  setInv <- function(invMat) inv <<- invMat   
  getInv <- function() inv
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}

###################################################
# This function computes the inverse of the       # 
# special "matrix" returned by makeCacheMatrix    #
# above. If the inverse has already been          #  
# calculated (and the matrix has not changed),    #
# then the cachesolve should retrieve the inverse #  
# from the cache.                                 #
###################################################
cacheSolve <- function(mat, ...) {
  cInv <- mat$getInv()
  if(!is.null(cInv)) {
    message("getting cached data")
    return(cInv)
  }
  data <- mat$get()
  cInv <- solve(data)
  mat$setInv(cInv)
  cInv
}

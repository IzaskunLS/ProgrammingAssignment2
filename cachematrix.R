##################################################################################
# This function creates a special "matrix" object that can cache its inverse.    #
#     Result is a list of 4 elements to keep:                                    #
#              1. set:    function to reset variables                            #
#              2. get:    Cache Original matrix                                  #                         
#              3. setinv: function to calculate inverse matrix                   #
#              4. getinv: result of setinv("matrix")                             #
##################################################################################

makeCacheMatrix <- function(or_matrix = matrix()) {
  
  # New matrix to reset inverse Matrix
  inv_matrix <- NULL
  # New objetc to cache a function which reset inverse matrix and keep original 
  # in object "set"
  set <- function(y){
    or_matrix <<- y
    inv_matrix  <<- NULL
  }
  
  # Create new object to cache the original matrix
  get <- function () or_matrix
  
  # Create an object to cache a function to calculate the inverse matrix
  setinv <- function(solve) inv_matrix <<- solve
  
  # Create an object to cache the result
  getinv <- function() inv_matrix
  
  # result of makeCacheMatrix()
  list(set=set, get = get,
       setinv =setinv, getinv = getinv)
}

##################################################################################
# This function calculates the inverse matrix:                                   #
#        1. Check if the inverse matrix is reset.                                #
#                 * true: continue                                               #
#                 * false: show an error message                                 #
#        2. Check if the original matrix is an squared matrix                    #
#                 * true: continue                                               #
#                 * false: show a warnning "not square matrix                    #
#        3. take the list obtained from makeCacheMatrix to calculate the inverse # 
#           matrix.                                                              #
#     Result is the inverse matrix                                               #
##################################################################################
cacheSolve <- function(l_inv_matrix, ...) {
  
  #Reset object to keep the inverse matrix
  inv_matrix <- l_inv_matrix$getinv()
  
  if(!is.null(inv_matrix)){
    message("getting cached data")
    return(inv_matrix)
  }
  
  #Object to keep the original matrix
  data<- l_inv_matrix$get()
  
  #Message if matrix is not squared
  if(nrow(data) == ncol(data)) {
    
    #Function to calculate inverse matrix
    inv_matrix <- solve(data, ...)
    
    #Execute function to calculate inv_matrix
    l_inv_matrix$setinv(inv_matrix)
    
    #result of cacheSolve
    inv_matrix
  }
  else{
    print ("Matrix must be square")
  }
  
}

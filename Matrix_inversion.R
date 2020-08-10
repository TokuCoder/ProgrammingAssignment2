# Coursera R Programming Week 3 Assignment 2
# Objective 1 : "makeCacheMatrix" This function creates a special "matrix" object that can cache its inverse
# Objective 2 : "cacheSolve" This function computes the inverse of the special "matrix" returned by
#               makeCacheMatrix above. 

makeCacheMatrix <- function(x= matrix()){
  # I am creating the function that creates a special "matrix" object that can cache its inverse.

  inversion <-  NULL
  # to set the default value at NULL. Will hold the value of matrix inverse.
  set <- function(y) {   #set the value of the matrix using another function
    x <<- y    # using the double arrow :  assigning value of matrix in parent environment
    inversion <<- NULL  # this basically resets "inversion" to "NULL" if there is a new matrix.
  }
  get <- function() x   # this get function returns the value of the mattrix argument
  
  set_inverse <- function(inverse) inversion <<- inverse  
  # using the double arrow the inversion value assigned to parent environment
  get_inverse <- function() inversion  
  # the get_inversion gets the value of the inversion where it is called 
  list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
  # By creating a list, we are able to use the $ operator to refer to the functions. 
}


# The cacheSolve function solves for the inverse of the matrix returned by the makeCacheMatrix created above.
# the cacheSolve will pull out the inverse from the cache if the matrix has been calculated. 
cacheSolve <- function (x,...) {
  
  inversion <- x$get_inverse()
  if(!is.null(inversion)) {
    message("getting cached data")
    return(inversion)
  }
  data <- x$get()
  inversion <- solve(data, ...)
  x$set_inverse(inversion)
  inversion
}

# We now use a simple two by two matrix as an example. 

pmatrix <- makeCacheMatrix(matrix((1:4), nrow = 2, ncol =2))
# we should get a table that looks like 
#     [,1] [,2]
#[1,]    1    3
#[2,]    2    4
pmatrix$get()
pmatrix$get_inverse()
cacheSolve(pmatrix)

# the inversed cache matrix should look as the following: 
#      [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
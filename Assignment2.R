##Assignment 2 - caching the inverse of a matrix
##first functioncreated matrix to cache inverse
##second function computes inverse of special matrix from first function

>makeCacheMatrix <- function(x = matrix()) {#x initialized with default empty matrix
  i <- NULL #initializing i in makeCacheMatrix envirnmen
  set <- function(y) {#
    x <<- y #assigns input y to matrix x in parent environment
    i <<<- NULL # reassigns i as NULL to clear any previously cached value
    }
  get <- function() x #retrieves matrix x from parent envirnment
  setinverse <- function(inverse) i <<- inverse # assigns new inverse value to i in parent environment
  getinverse <- function() i #retrieves inverse i from parent environment
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) #each function returned as named
  #object in list to be returned to parent envirnment
  }

> cacheSolve <- function(x, ...) {
  i <- x$getinverse() # calls getinverse function on input object x and assigns to i
  if(!is.null(i)){ #checks if inverse matrix is NULL (new matrix as input sets i to NULL)
    message("getting cached data")
    return(i) #returns the valid, cached inverse matrix if not NULL
    }
  data <- x$get() #if i is NULL (!is.null(i)=FALSE), get matrix from input
  i <- solve(data, ...) #calculate inverse of new matrix
  x$setinverse(i) #sets inverse of input matrix
  i #prints inverse matrix
  }

##test
> i <- matrix(c(1,2,3,4),2,2)
> m1 <- makeCacheMatrix(i)
> cacheSolve(m1)
    [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5

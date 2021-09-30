## This speciific R program aims to cache a matrix and consequently calculate its inverse via the
## the following set of function (makeCacheMatrix and cacheSolve).

## The following function sets user-input x as a given matrix.

library(MASS)
makeCacheMatrix <- function(x = matrix()){
  Inverse <- NULL ## This is to set the inverse as NULL
  Set <- function(y){ ## This aims to set the function to that of new
        x <<- y
        Inverse <<- NULL
                      }
  Obtain <- function() {x} ## This is the function used to get the matrix. Hence, obtain.
  SettingInv <-function(inverse)Inverse<<-inverse ## This essentially sets the inverse.
  GettingInv<-function(){       
                      inver<-ginv(x)
                      inver%*%x ## Obtain the inverse.
  }
  list(Set = Set, Obtain = Obtain, SettingInv = SettingInv, getinv =  GettingInv)
  
}

## The following function is used to obtain and then calculate/solve the inverse of a given matrix from a chache.

cacheSolve <- function(x, ...){
  Inverse <- x$GettingInv()
  if(!is.null(Inverse)){
                message("Currently Obtaining Matrix From The Cached Data...") ## This is to let users know that the data is being obtained. Additionally, it's used to check if NULL.
                return(Inverse)  ## Consequently returns the value/magnitude of the inverse.                     
  }
        mat <- x$get() ## mat function
        Inverse <-solve(mat, ...)  ## Finally the inverse is solved.            
        x$SettingInv(Inverse)
        Inverse
}

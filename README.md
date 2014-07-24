ProgrammingAssignment2

#############Assigmment 2############# 
makeCacheMatrix <- function(tio = numeric() ){
  m <- NULL
  set <- function(tia) {
    tio <<- tia
    m <<- NULL
  }
  get <- function() tio
  setmatrix <- function(matrix) m <<- matrix
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}

cacheSolve <- function(tio, ...) {
  m <- tio$getmatrix()
  if(!is.null(m)) {
  message("getting cached data")
  return(m)
  }
  data <- tio$get()
  dat<-matrix(data , ncol = sqrt(length(data)))
  m <- solve(dat, ...)
  tio$setmatrix(m)
  m
  }

tt <- rnorm(100,20)
uu <- makeCacheMatrix(tt)
cacheSolve(uu)


======================

abcdeabcde11234567891123456789aabbccddee1123456789

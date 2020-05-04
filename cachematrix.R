## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix<-function(x=matrix()){
  #setting the matrix to initially be null
  inv<-NULL
  #setting the value of matrix x
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  #getting the value of the matrix
  get<-function(){
    return(x)
  }
  #setting the inverse of the matrix (Setting in cache)
  setinv<-function(inverse){
    inv<<-inverse
  }
  #getting the inverse of the matrix (Getting from cache)
  getinv<-function() inv
  list(set=set,get=get,setinv=setinv,getinv=getinv)
}

cacheSolve<-function(x){
  #getting inverse
  inv<-x$getinv()
  #checking if inverse is null. if not, inverse unavaliable in cache
  if(!is.null(inv)){
    message("getting cache")
    return(inv)
  }
  #when inverse unavailable in cache calculating value of inverse to be stored in cache
  mat<-x$get()
  inv<-solve(mat)
  x$setinv(inv)
  return(inv)
}
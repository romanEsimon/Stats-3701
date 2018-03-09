
#'
#'gets inverse of matrix and mulitlpies it.
#'
#'
#'@param infile Path to the imput file
#'@return A matix of the infile
#'@export
#'@example
#'y<-c(1,2,3,2,4,5,3,5,6)
#'x<-matrix(y,nrow = 3,ncol = 3,byrow = TRUE)
#'q<-c(7,8,9)
#'a<-matrix(q,nrow = 3, ncol = 1, byrow = TRUE)
#'inv(x,a)
inv<-function(x,a){
stopifnot(length(x)!=0)##If there is noting in the vector x
stopifnot(length(a)!=0)
stopifnot(nrow(x)==ncol(x))#to check if the matrix is square.
stopifnot(is.numeric(x))#if data in X is not numeric
stopifnot(is.finite(x))#if for some reason the data is never ending
stopifnot(is.finite(a))
stopifnot(!is.na(x))##If any elements are missing
stopifnot(!is.nan(x))##if the vectors is NaN
stopifnot(!is.na(a))##If any elements are missing
stopifnot(!is.nan(a))
solve(x,a)
}
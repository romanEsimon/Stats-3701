
           #' Calculate Mean, Variane, SD (again)
           #' Computes the mean, variance and sd of a vector, but with user checks
           #' X must be numeric,have length greater than zero, is finite, is a number.
           #' @param x vector
           #'
           #' @return list
           #' @export
           #' @examples
           #' x<-c(1,2,3,4,5,6,7,8,9,10)
           #' stts(x)
           stts <- function(x){
           stopifnot(is.numeric(x))
           stopifnot(length(x)!=0)
           stopifnot(is.finite(x))
           stopifnot(!is.na(x))
           stopifnot(!is.nan(x))
           
           a = sum(x)/length(x)
           b = sum((x-a)^2)/length(x)
           c = sqrt(b)
           return(list(mean=a,var=b,sd=c))
           }
           
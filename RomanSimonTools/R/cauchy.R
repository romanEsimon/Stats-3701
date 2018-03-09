
           #' MLE of a Cauchy distribution
           #' Computes the liklihood of a Cauchy distribution
           #'
           #' @param x vector
           #'
           #' @return scalar
           #' @export
           #' @examples
           #' x<-c(1,2,3,4,5,6,7,8,9)
           #' cfun(x)
           cfun<-function(x){
           
           logl<-function(theta){
           sum(dcauchy(x, location = theta, log = TRUE))
           }  
           interval<-mean(x)+c(-10,10)*3*sd(x)
           
           interval<-pmax(mean(x)/1e3,interval)
           
           oouts<-optimize(logl,maximum = TRUE,interval)
           
           MAX<-oouts$maximum
           
           return(MAX)
           }
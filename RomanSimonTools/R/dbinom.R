
           #' MLE of a dbinom distribution
           #' Computes the liklihood of a dbinom distribution
           #'
           #' @param x vector
           #'
           #' @return scalar
           #' @export
           #' @examples
           #' x<-c(1,2,3,4,5,6,7,8,9)
           #' dFun(x)
           dFun<-function(x){
           
           logl<-function(theta){
           sum(dbinom(x, 20, prob = 1 / (1 + exp(- theta)), log = TRUE))
           }
           interval<-mean(x)+c(-1,1)*3*sd(x)
           
           interval<-pmax(mean(x)/1e3,interval)
           
           oouts<-optimize(logl,maximum = TRUE,interval)
           
           MAX<-oouts$maximum
           
           return(MAX)
           }
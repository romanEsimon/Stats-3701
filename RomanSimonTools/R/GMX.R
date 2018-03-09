
           #' MLE of gamma distribution
           #' Computes the liklihood of a gamma distribution
           #'
           #' @param x vector
           #'
           #' @return scalar
           #' @export
           #' @examples
           #' x<-c(1,2,3,4,5,6,7,8,9)
           #' func3(x)
           func3 <- function(x){
           alpha <- pi
           log <- function(alpha)
           sum(dgamma(x, shape = alpha, log = TRUE))
           interval <- mean(x) + c(-1,1) * 3 * sd(x)
           interval <- pmax(mean(x) / 1e3, interval)
           
           oout<- optimize(log, maximum = TRUE, interval)
           return (oout$maximum)
           }
           
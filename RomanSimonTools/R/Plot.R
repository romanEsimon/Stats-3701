
           #' Wrapper function for ggplot2 for data d
           #' Computes the mean, variance and sd of a vector
           #' @param x data.frame
           #' @return ggplot2
           #' @export
           #' @examples
           #' d<-c(2,1,3,4,6,7,78,8,8,8,87,77,6,434,6,5)
           #' data(d)
           #' plotMyData(d)
           plotMyData<-function(x){
           library(magrittr)
           x%>% ggplot2::ggplot()+ggplot2::aes(x=x, y=p)+ggplot2::geom_point()
           }
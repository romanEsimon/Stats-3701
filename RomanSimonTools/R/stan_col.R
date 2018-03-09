
           #'Takes a numeric matrix and standardizes its columns
           #'
           #'A matrix with only one row cannot be standardized
           #'so all matricies must have more than one row
           #'
           #'
           #'@return A matix 
           #'@export
           #'@example
           #' a<-c(16,10,24,11,3,18,9,14,6,17,8,23,22,19,1,12,4,5,20,2,13,7,15,21)
           #' x<-matrix(a, nrows=6, ncol=6, byrow=TRUE)          
           #'stan(x)                               
           stan<-function(x){
           
           stopifnot(length(x)!=0)
           
           stopifnot(nrow(x) > 1)
           
           stopifnot(is.finite(x))
           
           stopifnot(!is.na(x))
           
           stopifnot(!is.nan(x))
           
           x = (x - mean(x))/sd(x)
           
           }
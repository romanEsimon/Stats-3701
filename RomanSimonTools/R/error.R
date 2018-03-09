
           #'An error checking function
           #'
           #'This function serves the people to help better understand 
           #'how errors are caught in r. 
           #'
           #'
           #'@return 
           #'@export
           #'@example
           #'a<-Inf
           #'b<-NaN
           #'c<-NA
           #'d<-(NULL)
           #'e<-('e')
           #'error(a,b,c,d,e)
           error<-function(A,B,C,D,E){
           
           stopifnot(!is.numeric(E))#if data in X is not numeric 
           
           stopifnot(length(D)==0)##If there is noting in the vector x
           
           stopifnot(!is.finite(A))#if for some reason the data is never ending
           
           stopifnot(is.na(C))##If any elements are missing 
           
           stopifnot(is.nan(B))##if the vectors is Not a number 
           }
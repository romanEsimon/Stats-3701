
           #'This is a function version of apply
           #'The three argumnets is the array, the margin (1 or 2), and the function
           #' 
           #'@return array
           #'@export
           #'examples
           #'x<-c(1,2,3,4,5,6,7,8,9)
           #'X<-array(x,dim = 2)
           #'myapply(X,1,sum)
           myapply<-function(X,Margin,fun,..){
           
           if(length(dim(X))!=2)
           {
           stop('Matrix is not 2d')  
           }
           if(!(MARGIN%in%c(1,2)))
           {
           stop('margin is not 1 or 2')
           }
           R = dim(X)[1]
           C = dim(X)[2]
           f - match.fun(FUN)
           
           if(MARGIN == 1){
           
           result =  list()
           for(i in 1:R){
           result[[i]]= f(X[i,],...)
           }
           }
           else if (MARGIN == 2){
           result= list()
           for (j in 1:C )
           {
           result[[j]]=f(X[,j])
           }
           }
           
           return(simplify2array(result))
           }
test_that("T_inv",{
  y<-c(1,2,3,2,4,5,3,5,6)
  x<-matrix(y,nrow = 3,ncol = 3,byrow = TRUE)
  q<-c(7,8,9)
  a<-matrix(q,nrow = 3, ncol = 1, byrow = TRUE)
  b<-solve(x,a)

  expect_equal(inv(x,a), b)

})

test_that("Simple_stats",{
  x<-c(1,2,3,4,5,6,7,8,9,10)
  tstts<- function(x){
    stopifnot(is.numeric(x))
    stopifnot(length(x)!=0)
    stopifnot(is.finite(x))
    stopifnot(!is.na(x))
    stopifnot(!is.nan(x))

    a = sum(x)/length(x)
    b = sum((x-a)^2)/length(x)
    c = sqrt(b)
    return(list(mean=a,var=b,sd=c))}

  expect_equal(tstts(x), stts(x))
})

test_that("MLEgamma",{
  x<-c(1,2,3,4,5,6,7,8,9)
    gfun <- function(x){
    alpha <- pi
    log <- function(alpha)
    sum(dgamma(x, shape = alpha, log = TRUE))
    interval <- mean(x) + c(-1,1) * 3 * sd(x)
    interval <- pmax(mean(x) / 1e3, interval)

    oout<- optimize(log, maximum = TRUE, interval)
    return (oout$maximum)}

  expect_equal(func3(x), gfun(x))

})

test_that("dbinom",{
  x<-c(1,2,3,4,5,6,7,8,9)
  dFun1<-function(x){

    logl<-function(theta){
      sum(dbinom(x, 20, prob = 1 / (1 + exp(- theta)), log = TRUE))
    }
    interval<-mean(x)+c(-1,1)*3*sd(x)

    interval<-pmax(mean(x)/1e3,interval)

    oouts<-optimize(logl,maximum = TRUE,interval)

    MAX<-oouts$maximum

    return(MAX)
  }
  expect_equal(dfun(x),dFun1(x))
})

test_that(" Cauchy",{
  x<-c(1,2,3,4,5,6,7,8,9)
  cfun1<-function(x){

    logl<-function(theta){
      sum(dcauchy(x, location = theta, log = TRUE))
    }
    interval<-mean(x)+c(-10,10)*3*sd(x)

    interval<-pmax(mean(x)/1e3,interval)

    oouts<-optimize(logl,maximum = TRUE,interval)

    MAX<-oouts$maximum


    expect_equal(cfun(x),cfun1(x))
  }
})

test_that("Errors",{
  a<-Inf
  b<-NaN
  c<-NA
  d<-(NULL)
  e<-("e")

  stts1<-function(A,B,C,D,E){
    stopifnot(!is.numeric(E))#if data in X is not numeric

    stopifnot(length(D)==0)##If there is noting in the vector x

    print("In progress")

    stopifnot(!is.finite(A))#if for some reason the data is never ending

    stopifnot(is.na(C))##If any elements are missing

    stopifnot(is.nan(B))##if the vectors is Not a number
  }
  stts(a,b,c,d,e)

  expect_equal(error(a,b,c,d,e),stts1(a,b,c,d,e))

})

test_that("%MLE%",{
  "%fun%"<-function(x){
    logl<-function(theta){
      sum(dgamma(x,shape=theta,log = TRUE))
    }
    interval <-mean(x)+c(-1,1)*3*sd(x)

    interval <-pmax(mean(x)/1e3,interval)

    print(interval)

    oouts<-optimize(logl,maximum = TRUE,interval)

    MAX<-oouts$maximum

    return(MAX)}

  expect_equal(fun(x), gun(x))

})

test_that("myapply",{
  x<-c(1,2,3,4,5,6,7,8,9)
  myapply1<-function(X,Margin,fun,..){

    if(length(dim(X))!=2)
    {
      stop("Matrix is not 2d")
    }
    if(!(MARGIN%in%c(1,2)))
    {
      stop("margin is not 1 or 2")
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
  expect_equal(myapply1(x,1,sum),myapply(x,1,sum))
})

test_that("T_stan ", {
  a<-c(16,10,24,11,3,18,9,14,6,17,8,23,22,19,1,12,4,5,20,2,13,7,15,21)
  x<-matrix(a, ncol=4, byrow=TRUE)
  stan1<-function(x){
    
    stopifnot(length(x)!=0)
    
    stopifnot(nrow(x) > 1)
    
    stopifnot(is.finite(x))
    
    stopifnot(!is.na(x))
    
    stopifnot(!is.nan(x))
    
    x = (x - mean(x))/sd(x)
  }
  stan1(x)
  
  expect_equal(stan(x), stan1(x))
  
})

devtools::check(loc)
getwd()

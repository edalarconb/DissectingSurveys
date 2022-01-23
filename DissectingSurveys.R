
options(scipen = 999)


# libraries to be used
library(foreign)
library(dplyr)
library(haven)
library(rio)

# Cumulative distribution function
q <- function(a)
{
  if (class(a)!='numeric'
    {'The input "a" must be numeric class'}
else{
  a.sort <- sort(a)
  q.<-c()
    for(i in 1:length(a.sort))
      {
        q.[i]=sum(a.sort<=a.sort[i])/length(a.sort)
      }
  data.ordered=data.frame(x=a.sort,proportion=q.)
  e_cdf=distinct(data.ordered,x,.keep_all = TRUE)
  return(e_cdf)}
}


# Quantile function
quantile.f <-function(data,alpha)
{
  data.class=class(data)
  dim.data=dim(data)[2]
  if (data.class!="data.frame" |
      dim.data!=2){'The input "data" must be data.frame class with two columns'}
  else{
       q.function=lapply(1:length(alpha),
                    function(a) min(which(data[,2]>=alpha[a])))
  positions=unlist(q.function)
  quantile.x=data[positions,][,1]
  return(quantile.x)}
}

#loading the data

data.casen=read.spss("Casen en Pandemia 2020 SPSS.sav",
                     to.data.frame = TRUE)

## salaried categories 
cat.y1_preg=names(table(data.casen %>% select(y1_preg)))
cat.asalariados=cat.y1_preg[c(1,3)] 
asalariados=data.casen %>%  
  filter(y1_preg %in% cat.asalariados)

## imputed and non-imputed salaried data
as.sinImp=na.omit(asalariados$y0101) 
resp=length(as.sinImp)/dim(asalariados)[1]
n.resp=1-resp

as.Imp=na.omit(asalariados$y0101c)

#compute quantiles with the q function
quantiles.SinImp=q(as.sinImp) ## quantiles in Z=1
quantiles.Imp=q(as.Imp) ## quantiles in imputed incomes

### Table 2
alpha=c(0.05,0.1,0.25,0.5,0.75,0.9,0.95,0.99)
q.sinImp=quantile.f(quantiles.SinImp,alpha=alpha)
q.Imp=quantile.f(quantiles.Imp,alpha=alpha)

data.frame(alpha,
           original_quantile=q.sinImp,
           imputed_quntile=q.Imp)

### incomes avarage in Z=1 and imputed incomes
mean(as.sinImp)
mean(as.Imp)

# Figure 1
plot(quantiles.SinImp$x,quantiles.SinImp$proportion,
     xlim=c(0,3000000),
     xlab="Incomes",
     ylab="Cumulative distribution",
     cex=0.5)

# Figure 2
plot(quantiles.SinImp$x,quantiles.SinImp$proportion,
     xlim=c(0,3000000),
     xlab="Incomes",
     ylab="Cumulative distribution",
     cex=0.5)
points(quantiles.SinImp$x,quantiles.SinImp$proportion*resp,
       col="blue",
       cex=0.5)
points(quantiles.SinImp$x,quantiles.SinImp$proportion*resp+(1-resp),
       col="red",
       cex=0.5)
legend(x = "bottomright", pch=16, inset=.01,
       col= c("red","blue","black"), 
       legend=c("Lower bound", 
                "Upper bound",
                "Observed income distribution"),
       cex=0.7)

# Figure 3
plot(quantiles.SinImp$x,quantiles.SinImp$proportion,
     xlim=c(0,3000000),
     xlab="Incomes",
     ylab="Cumulative distribution",
     cex=0.5)
points(quantiles.SinImp$x,quantiles.SinImp$proportion*resp,
       col="blue",
       cex=0.5)
points(quantiles.SinImp$x,quantiles.SinImp$proportion*resp+(1-resp),
       col="red",
       cex=0.5)
points(quantiles.Imp$x,quantiles.Imp$proportion,
       col="green",
       cex=0.5)
legend(x = "bottomright", pch=16, inset=.01,
       col= c("red","blue","black","green"), 
       legend=c("Lower bound", 
                "Upper bound",
                "Observed income distribution",
                "Imputed income distribution"),
       cex=0.7)

#### Table 4
alpha=c(0.05,0.10,n.resp,0.25,0.5,0.75,
        0.8,resp,0.9,0.95,0.99,1)

L.alpha=(alpha-n.resp)/resp
U.alpha=alpha/resp

L.alpha[which(L.alpha<0)]=0
U.alpha[which(U.alpha>1)]=1

q.sinImp=quantile.f(quantiles.SinImp,
                    alpha=alpha)

q.lowerB=quantile.f(quantiles.SinImp,
                    alpha=L.alpha)
q.upperB=quantile.f(quantiles.SinImp,
                    alpha=U.alpha)


data.frame(alpha, 
           LB.q=q.lowerB,
           UB.q=q.upperB,
           imputed.q=quantile.f(quantiles.Imp,alpha))



###################################
###################################

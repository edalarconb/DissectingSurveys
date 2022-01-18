# Cumulative distribution function
q <- function(a)
{
  a.sort <- sort(a)  
  q.<-c()
  for(i in 1:length(a.sort))
  {
    q.[i]=sum(a.sort<=a.sort[i])/length(a.sort)
  }
  data.ordered=data.frame(x=a.sort,proportion=q.)
  e_cdf=distinct(data.ordered,x,.keep_all = TRUE)
  return(e_cdf)
}

# quantile function
quantile.f <-function(data,alpha)
{
  q.function=lapply(1:length(alpha),
                    function(a) min(which(data[,2]>=alpha[a])))
  positions=unlist(q.function)
  quantile.x=data[positions,][,1]
  return(quantile.x)
}

# libraries to be used
library(foreign)
library(dplyr)
library(haven)
library(rio)

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
resp=round(length(as.sinImp)/dim(asalariados)[1],3)
n.resp=1-resp

as.Imp=na.omit(asalariados$y0101c)

mean(as.sinImp)
mean(as.Imp)

#compute quantiles with the q function
quantiles.SinImp=q(as.sinImp) ## this is Z=1
quantiles.Imp=q(as.Imp)

### quantiles table 
alpha=c(0.05,0.1,0.25,0.5,0.5,0.75,0.9,0.95,0.99)
quantile.f(quantiles.SinImp,alpha=alpha)
quantile.f(quantiles.Imp,alpha=alpha)

####### plots
# P(Y<=y|Z=1,C=1,S=1)
plot(quantiles.SinImp$x,quantiles.SinImp$proportion,
     xlim=c(0,3000000),
     xlab="Incomes",
     ylab="Cumulative distribution",
     cex=0.5)

# Identification region for P(Y<=y|C=1,S=1)
p=resp
plot(quantiles.SinImp$x,quantiles.SinImp$proportion,
     xlim=c(0,3000000),
     xlab="Incomes",
     ylab="Cumulative distribution",
     cex=0.5)
points(quantiles.SinImp$x,quantiles.SinImp$proportion*p,
       col="blue",
       cex=0.5)
points(quantiles.SinImp$x,quantiles.SinImp$proportion*p+(1-p),
       col="red",
       cex=0.5)
legend(x = "topleft", pch=16, inset=.01,
       col= c("red","blue","black"), 
       legend=c("Lower bound", 
                "Upper bound",
                "Observed income distribution"),
       cex=0.7)

#### quantile function
alpha=quantiles.SinImp$proportion
alpha.id=seq(n.resp,resp,length=100)

L.alpha=(alpha.id-n.resp)/resp
U.alpha=alpha.id/resp
q.sinImp=quantile.f(quantiles.SinImp,
                    alpha=alpha)
q.lowerB=quantile.f(quantiles.SinImp,
                    alpha=L.alpha)
q.upperB=quantile.f(quantiles.SinImp,
                    alpha=U.alpha)


plot(alpha,q.sinImp,ylim=c(0,3000000))
points(alpha.id,q.lowerB,col="red")
points(alpha.id,q.upperB,col="blue")

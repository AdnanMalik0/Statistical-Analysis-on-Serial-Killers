# Coursework (Deciding Model)
# Run Coursework0.R before

# AgeFirstKill -- GANG
sample=gang
hist(sample$AgeFirstKill,freq=FALSE,breaks=seq(10,50,by=1),xlab=sample$Motive[1],main=paste("Histogram of killer's Age at first kill"),col="lightyellow",border=TRUE)
lines(seq(0,100,0.02),dnorm(seq(0,100,0.02),mean=mean(sample$AgeFirstKill),sd=sd(sample$AgeFirstKill)),lwd=2,col="darkgreen")
# KS Test ## PASSED
ks.test(x =  sample$AgeFirstKill, y = "pnorm", mean=mean(sample$AgeFirstKill), sd=sd(sample$AgeFirstKill))
# QQ-Plot test ## PASSED
mu <- mean(sample$AgeFirstKill) 
sigma <- sd(sample$AgeFirstKill)
# Standardised order statistics:
z <- (sort(sample$AgeFirstKill) - mu)/sigma
n <- length(sample$AgeFirstKill)
r <- (1:n)
# Quantiles of N(0, 1):  
q <- qnorm(p = r/(n + 1), mean = 0, sd = 1)
# A normal "QQ plot": 
?plot
plot(q, z,xlab="Gang, cult or Organised crime", main="QQ-plot of killer's Age at first kill")
abline(a=0,b=1,col='darkgreen',lwd=1.5)
# Chi-squared test ## PASSED
library(nortest)
pearson.test(sample$AgeFirstKill)
## gang -> Normally distributed

# AgeFirstKill -- AOD
sample=aod 
hist(sample$AgeFirstKill,freq=FALSE,breaks=seq(10,70,by=3),xlab=sample$Motive[1],main=paste("Histogram of killer's Age at first kill"),col="lightgray",border=TRUE)
lines(seq(0,100,by=0.02),dnorm(seq(0,100,by=0.02),mean=mean(sample$AgeFirstKill),sd=sd(sample$AgeFirstKill)),lwd=2,col="blue")
# KS Test ## PASSED
ks.test(x =  sample$AgeFirstKill, y = "pnorm", mean=mean(sample$AgeFirstKill), sd=sd(sample$AgeFirstKill))
# QQ-Plot test ## PASSED
mu <- mean(sample$AgeFirstKill) 
sigma <- sd(sample$AgeFirstKill)
# Standardised order statistics:
z <- (sort(sample$AgeFirstKill) - mu)/sigma
n <- length(sample$AgeFirstKill)
r <- (1:n)
# Quantiles of N(0, 1):  
q <- qnorm(p = r/(n + 1), mean = 0, sd = 1)
# A normal "QQ plot": 
plot(q, z,xlab="Angel of Death", main="QQ-plot of killer's Age at first kill")
abline(a=0,b=1,col='blue',lwd=1.5)
# Chi-squared test ## PASSED
library(nortest)
pearson.test(sample$AgeFirstKill)
## aod - Normally Distributed

# AgeFirstKill -- ESCAPE OR AVOID ARREST
sample=esc
hist(sample$AgeFirstKill,freq=FALSE,breaks=seq(10,70,by=3),xlab=sample$Motive[1],main=paste("Histogram of killer's Age at first kill"),col="lightblue",border=TRUE)
lines(seq(0,100,by=0.02),dnorm(seq(0,100,by=0.02),mean=mean(sample$AgeFirstKill),sd=sd(sample$AgeFirstKill)),lwd=2,col="red")
lines(seq(0,100,by=0.02),dexp(seq(0,100,by=0.02),rate=1/mean(sample$AgeFirstKill)),lwd=2,col="red")
# KS Test ## PASSED
ks.test(x =  sample$AgeFirstKill, y = "pnorm", mean=mean(sample$AgeFirstKill), sd=sd(sample$AgeFirstKill))
# QQ-Plot test ## PASSED
mu <- mean(sample$AgeFirstKill) 
sigma <- sd(sample$AgeFirstKill)
# Standardised order statistics:
z <- (sort(sample$AgeFirstKill) - mu)/sigma
n <- length(sample$AgeFirstKill)
r <- (1:n)
# Quantiles of N(0, 1):  
q <- qnorm(p = r/(n + 1), mean = 0, sd = 1)
# A normal "QQ plot": 
plot(q, z,xlab="Escape or avoid arrest", main="QQ-plot of killer's Age at first kill")
abline(a=0,b=1,col='red',lwd=1.5)

# Chi-squared test ## PASSED
library(nortest)
pearson.test(sample$AgeFirstKill)

# esc - Normally distributed

## Up until-now it has been seen that all the three datasets
## are Normally distributed. therefore it is assumed that they
## come from normally distributed population.


# Coursework (checking mu=27 for each motive)
# Quality check
# All the motives and their records

# Quality check
for (i in c('Angel of Death','Gang, cult or organised crime','Escape or avoid arrest')){
  len0=length(mysample[mysample$Motive==i,"AgeFirstKill"])
  nulls0=sum(is.na(mysample[mysample$Motive==i,"AgeFirstKill"]))
  specials0=length(mysample[mysample$Motive==i & mysample$AgeFirstKill>110,"AgeFirstKill"])
  years0=length(mysample[mysample$Motive==i & mysample$YearAtFirstKill<1900,"AgeFirstKill"])
  expectedlength=len0-nulls0-specials0-years0
  if (i=="Angel of Death"){
    sample=aod
  }
  else if(i=="Gang, cult or organised crime"){
    sample=gang
  }
  else{
    sample=esc
  }
  flag=length(sample$AgeFirstKill)==expectedlength
  if (flag){
    print(c("All seems good for",i))
  }
  else{
    print(c("Issues with",i))
  }
}


boxplot(mysample0$AgeFirstKill ~ mysample0$Motive,type=1, range=5) # Range to extend the box


# Hypothesis testing
# Is pop. mean of aod, esc and gang equal to 27?
# defining a function to reduce hassle
ztest=function(x,mu,sigma2=sd(x)^2,siglevel){
  xbar=mean(x)
  n=length(x)
  alphaby2=siglevel/2
  CI=xbar + c(-1,1)*qnorm(p=(1-alphaby2),mean=0,sd=1)*sqrt(sigma2/n)
  z=(xbar-mu)/sqrt(sigma2/n)
  pval=2*pnorm(q=-z,mean=0,sd=1)
  return(c(CI,pval,z))
}

ttest=function(x,mu,siglevel){
  xbar=mean(x)
  n=length(x)
  samplevar=sd(x)^2
  alphaby2=siglevel/2
  CI=xbar + c(-1,1)*qt(p=(1-alphaby2),df=n-1)*sqrt(samplevar/n)
  T=(xbar-mu)/sqrt(samplevar/n)
  pval=2*pt(q=-(T),df=n-1)
  print(c("CI",CI))
  print(c("t",T))
  print(c("p-value",pval))
}

# ---- # # ---- # # ---- # # ---- # # ---- # # ---- # # ---- #
## MAIN ## For Angel of Death (aod)
# Ho: mu=27 Vs H1: mu!=27

# Assumptions
# 1. the sample is assumed as an IID sample
# 2. Normal distribution is assumed (supported by our test above)
# 3. sigma^2 unknown (sigma2=samplevar)
# Therefore we proceed with t-test

x=aod$AgeFirstKill
ttest(x,mu=27,siglevel=0.05)
# CI: (28.58,36.11) does'nt contain 27
t.test(x,mu=27,siglevel=0.05) # Confirmation

# Therefore proof against null

# ---- # # ---- # # ---- # # ---- # # ---- # # ---- # # ---- #
## MAIN ## For Gang, cult or organised crime (gang)
# Ho: mu=27 Vs H1: mu!=27

# (this is actual) Assumptions
# 1. the sample is assumed as an IID sample
# 2. Normal distribution is assumed (supported by our test above)
# 3. sigma^2 unknown (sigma2=samplevar)
# Therefore we proceed with t-test

x=gang$AgeFirstKill 
ttest(x,mu=27,siglevel=0.05)
# CI: (23.035,25.115) does'nt contain 27
t.test(x,mu=27) # Confirmation 

# Therefore proof against null

# ---- # # ---- # # ---- # # ---- # # ---- # # ---- # # ---- #
## MAIN ## For Escape or avoid arrest
# Ho: mu=27 Vs H1: mu!=27

# (this is actual) Assumptions
# 1. the sample is assumed as an IID sample
# 2. Normal distribution is assumed (supported by our test above)
# 3. sigma^2 unknown (sigma2=samplevar)
# Therefore we proceed with t-test

x=esc$AgeFirstKill
ttest(x,mu=27,siglevel=0.05)
# CI: (30.23,36.85) does'nt contain 27
t.test(x,mu=27) # Confirmation
# Therefore proof against null


# Therefore proof against null

# ---- # # ---- # # ---- # # ---- # # ---- # # ---- # # ---- #
# ---- # # ---- # # ---- # # ---- # # ---- # # ---- # # ---- #


# Coursework (Checking if mu1=mu2, m2=m3, mu3=mu1)

boxplot(mysample0$AgeFirstKill~mysample0$Motive) 
# Ho: mu(aod)-mu(gang)=0 Vs H1: mu(aod)-mu(gang)!=0
x1=aod$AgeFirstKill
x2=gang$AgeFirstKill

# Assumptions
# 1. Normality is assumed for both the samples
# 2. both are independent samples and are independent within themselves
1/3<sd(x1)/sd(x2)
sd(x1)/sd(x2)<3
# 3. based on the check above The pop. variance are considered same for both
# 4. Significance level is 0.05

x1barx2bar=mean(x1)-mean(x2)
s12=sd(x1)^2
s22=sd(x2)^2
n1=length(x1)
n2=length(x2)
serror=sqrt((1/n1)+(1/n2))
df=n1+n2-2
tn1n2=qt(0.975,df=df)
sp2=((n1-1)*s12 + (n2-1)*s22)/(n1+n2-2)

T=x1barx2bar/(sqrt(sp2)*serror)
CI=x1barx2bar+c(-1,1)*tn1n2*sqrt(sp2)*serror
pval=2*pt(q=-T,df=df)
CI
T
pval
t.test(x=x1,y=x2,mu=0,var.equal=TRUE,paired=FALSE) # Confirmation
# The avg. age seem to differ between killers with motive
# Angel of Death and those associated with Gang, cult or organised crime

# --- --- # --- --- # --- --- # # --- --- # --- --- # --- --- #

# Ho: mu(aod)-mu(esc)=0 Vs H1: mu(aod)-mu(esc)!=0
x1=aod$AgeFirstKill
x2=esc$AgeFirstKill

# Assumptions
# 1. Normality is assumed for both the samples
# 2. both are independent samples and are independent within themselves
1/3<sd(x1)/sd(x2)
sd(x1)/sd(x2)<3
# 3. based on the check above The pop. variance are considered same for both
# 4. Significance level is 0.05

x1barx2bar=mean(x1)-mean(x2)
s12=sd(x1)^2
s22=sd(x2)^2
n1=length(x1)
n2=length(x2)
serror=sqrt((1/n1)+(1/n2))
df=n1+n2-2
tn1n2=qt(0.975,df=df)
sp2=((n1-1)*s12 + (n2-1)*s22)/(n1+n2-2)

T=x1barx2bar/(sqrt(sp2)*serror)
pval=2*pt(q=-T,df=df)
CI=x1barx2bar+c(-1,1)*tn1n2*sqrt(sp2)*serror
CI
T
pval
t.test(x=x1,y=x2,mu=0,var.equal=TRUE) # Confirmation
# The avg. age does'nt seem to differ between killers with motive
# Angel of Death and those associated with Escape or avoid arrest

# --- --- # --- --- # --- --- # # --- --- # --- --- # --- --- #

# Ho: mu(aod)-mu(esc)=0 Vs H1: mu(aod)-mu(esc)!=0
x1=esc$AgeFirstKill
x2=gang$AgeFirstKill

# Assumptions
# 1. Normality is assumed for both the samples
# 2. both are independent samples and are independent within themselves
1/3<sd(x2)/sd(x1)
sd(x2)/sd(x1)<3
# 3. based on the check above The pop. variance are considered same for both
# 4. Significance level is 0.05

x1barx2bar=mean(x1)-mean(x2)
s12=sd(x1)^2
s22=sd(x2)^2
n1=length(x1)
n2=length(x2)
serror=sqrt((1/n1)+(1/n2))
df=n1+n2-2
tn1n2=qt(0.975,df=df)
sp2=((n1-1)*s12 + (n2-1)*s22)/(n1+n2-2)

T=x1barx2bar/(sqrt(sp2)*serror)
pval=2*pt(q=-T,df=df)
CI=x1barx2bar+c(-1,1)*tn1n2*sqrt(sp2)*serror
CI
T
pval
t.test(x=x1,y=x2,mu=0,var.equal=TRUE) # Confirmation
# The avg. age seem to differ between killers with motive
# Escape or avoid arrest and those associated with Gang, cult or organised crime



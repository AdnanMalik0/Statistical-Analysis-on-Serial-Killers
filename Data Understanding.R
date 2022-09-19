load("killersandmotives.Rdata")
createsample(x=22)


## DATA CLEANING ##
# Clearing AgeFirstKill from special vals
tot_obs=length(mysample$AgeFirstKill)
mysample0=mysample[mysample$AgeFirstKill<150,]
summary(mysample0)

# Calculating Career Duration
mysample0$CareerDuration=mysample0$AgeLastKill - mysample0$AgeFirstKill

# Calculating Year at first kill - MySample
mysample$YearAtFirstKill=mysample$YearBorn + mysample$AgeFirstKill

# Calculating Year at first kill - MySample0
mysample0$YearAtFirstKill=mysample0$YearBorn + mysample0$AgeFirstKill
mysample0=mysample0[mysample0$YearAtFirstKill>1900,]
sum(mysample0$YearAtFirstKill<1900)

# Checking Columns for NA
for (i in names(mysample0)){
  if(sum(is.na(mysample0[i]))>0){
    print(i)
  }
}

## Motive, Sentence and InsanityPlea has NAs.
## Not considering Sentence and InsanityPlea as for now

# Removing NAs from Motive
sum(is.na(mysample0$Motive))
mysample0=mysample0[!is.na(mysample0$Motive),]
sum(is.na(mysample0$Motive))

# Getting observations removed
rem_obs=length(mysample0$AgeFirstKill)
del_obs=tot_obs - rem_obs
# Number of observations removed
print(del_obs)
# Percentage observations removed
(del_obs/tot_obs)*100

## DATA EXPLORATION ##
hist(mysample0$AgeFirstKill,freq=FALSE)
hist(mysample0$AgeLastKill,freq=FALSE)
hist(mysample0[mysample0$CareerDuration>0,"CareerDuration"])
summary(mysample0$AgeFirstKill)
summary(mysample0$AgeLastKill)
summary(mysample0$CareerDuration)
quantile(mysample0$AgeFirstKill,type=1)
quantile(mysample0$AgeLastKill,type=1)
quantile(mysample0[mysample0$CareerDuration>0,"CareerDuration"],type=1)
# Correlation
cor(mysample0$AgeFirstKill,mysample0$AgeLastKill)
plot(mysample0$AgeFirstKill,mysample0$AgeLastKill)

mysample1=mysample0[mysample0$CareerDuration>0,]
cor(mysample1$AgeFirstKill,mysample1$CareerDuration)
plot(mysample1$AgeFirstKill,mysample1$CareerDuration)

cor(mysample1$AgeLastKill,mysample1$CareerDuration)
plot(mysample1$AgeLastKill,mysample1$CareerDuration)
# Some additional Exploration // Effect of Motive
unique(mysample0$Motive)
gang=mysample0[mysample0$Motive=="Gang, cult or organised crime",c("Motive","AgeFirstKill","AgeLastKill","CareerDuration")]
aod=mysample0[mysample0$Motive=="Angel of Death",c("Motive","AgeFirstKill","AgeLastKill","CareerDuration")]
esc=mysample0[mysample0$Motive=="Escape or avoid arrest",c("Motive","AgeFirstKill","AgeLastKill","CareerDuration")]

asian=mysample0[mysample0$Race=="Asian",c("Race","AgeFirstKill","AgeLastKill","CareerDuration")]
hisp=mysample0[mysample0$Race=="Hispanic",c("Race","AgeFirstKill","AgeLastKill","CareerDuration")]
black=mysample0[mysample0$Race=="Black",c("Race","AgeFirstKill","AgeLastKill","CareerDuration")]
white=mysample0[mysample0$Race=="White",c("Race","AgeFirstKill","AgeLastKill","CareerDuration")]

male=mysample0[mysample0$Sex=="Male",c("Sex","AgeFirstKill","AgeLastKill","CareerDuration")]
female=mysample0[mysample0$Sex=="Female",c("Sex","AgeFirstKill","AgeLastKill","CareerDuration")]

length(aod$AgeFirstKill)
length(gang$AgeFirstKill)
length(esc$AgeFirstKill)

length(asian$AgeFirstKill)
length(hisp$AgeFirstKill)
length(black$AgeFirstKill)
length(white$AgeFirstKill)

length(male$AgeFirstKill)
length(female$AgeFirstKill)

print("### Hispanic ###")
print("Age First Kill")
quantile(hisp$AgeFirstKill,type=1)
IQR(hisp$AgeFirstKill,type=1)
print("Age Last Kill")
quantile(hisp$AgeLastKill,type=1)
IQR(hisp$AgeLastKill,type=1)
print("Career Duration")
quantile(hisp$CareerDuration,type=1)
IQR(hisp$CareerDuration,type=1)


print("### Asian ###")
print("Age First Kill")
quantile(asian$AgeFirstKill,type=1)
IQR(asian$AgeFirstKill,type=1)
print("Age Last Kill")
quantile(asian$AgeLastKill,type=1)
IQR(asian$AgeLastKill,type=1)
print("Career Duration")
quantile(asian$CareerDuration,type=1)
IQR(asian$CareerDuration,type=1)


print("### Black ###")
print("Age First Kill")
quantile(black$AgeFirstKill,type=1)
IQR(black$AgeFirstKill,type = 1)
print("Age Last Kill")
quantile(black$AgeLastKill,type=1)
IQR(black$AgeLastKill,type=1)
print("Career Duration")
quantile(black$CareerDuration,type=1)
IQR(black$CareerDuration,type=1)




print("### White ###")
print("Age First Kill")
quantile(white$AgeFirstKill,type=1)
IQR(white$AgeFirstKill,type = 1)
print("Age Last Kill")
quantile(white$AgeLastKill,type=1)
IQR(white$AgeLastKill,type=1)
print("Career Duration")
quantile(white$CareerDuration,type=1)
IQR(white$CareerDuration,type=1)


print("### Male ###")
print("Age First Kill")
quantile(male$AgeFirstKill,type=1)
IQR(male$AgeFirstKill,type = 1)
print("Age Last Kill")
quantile(male$AgeLastKill,type=1)
IQR(male$AgeLastKill,type=1)
print("Career Duration")
quantile(male$CareerDuration,type=1)
IQR(male$CareerDuration,type=1)


print("### Female ###")
print("Age First Kill")
quantile(female$AgeFirstKill,type=1)
IQR(female$AgeFirstKill,type = 1)
print("Age Last Kill")
quantile(female$AgeLastKill,type=1)
IQR(female$AgeLastKill,type=1)
print("Career Duration")
quantile(female$CareerDuration,type=1)
IQR(female$CareerDuration,type=1)

print("### AOD ###")
print("Age First Kill")
quantile(aod$AgeFirstKill,type=1)
IQR(aod$AgeFirstKill,type = 1)
print("Age Last Kill")
quantile(aod$AgeLastKill,type=1)
IQR(aod$AgeLastKill,type=1)
print("Career Duration")
quantile(aod$CareerDuration,type=1)
IQR(aod$CareerDuration,type=1)

print("### GANG ###")
print("Age First Kill")
quantile(gang$AgeFirstKill,type=1)
IQR(gang$AgeFirstKill,type = 1)
print("Age Last Kill")
quantile(gang$AgeLastKill,type=1)
IQR(gang$AgeLastKill,type=1)
print("Career Duration")
quantile(gang$CareerDuration,type=1)
IQR(gang$CareerDuration,type=1)

print("### ESC ###")
print("Age First Kill")
quantile(esc$AgeFirstKill,type=1)
IQR(esc$AgeFirstKill,type = 1)
print("Age Last Kill")
quantile(esc$AgeLastKill,type=1)
IQR(esc$AgeLastKill,type=1)
print("Career Duration")
quantile(esc$CareerDuration,type=1)
IQR(esc$CareerDuration,type=1)
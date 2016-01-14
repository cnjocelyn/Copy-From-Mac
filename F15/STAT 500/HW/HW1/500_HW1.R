##Load the library
library(faraway)

##read in the data
data(uswages)
attach(uswages)
uswages
summary(uswages)

##summarize areas
area<-vector(mode = "character",length=0)
for(i in 1:2000){
 if(ne[i]==1)
  area[i]<-"NE"
 else if(mw[i]==1)
   area[i]<-"MW"
 else if(so[i]==1)
   area[i]<-"WE"
 else 
   area[i]<-"SO"
}

##Careforial Variable
uswages$race=factor(uswages$race)
uswages$smsa=factor(uswages$smsa)
uswages$ne=factor(uswages$ne)
uswages$mw=factor(uswages$mw)
uswages$we=factor(uswages$we)
uswages$so=factor(uswages$so)
uswages$pt=factor(uswages$pt)
summary.factor(race)
levels(uswages$race)=c("white","black")
levels(uswages$smsa)=c("no_smsa","smsa")
levels(uswages$ne)=c("no_ne","ne")
levels(uswages$mw)=c("no_mw","mw")
levels(uswages$we)=c("no_we","we")
levels(uswages$so)=c("no_so","so")
levels(uswages$pt)=c("no_pt","pt")
summary(uswages)

boxplot(wage~race,data=uswages,log='y',boxwex=0.35,col="lightgrey",main="Wages in Different Races",ylab="wage",xlab="race")
boxplot(wage~area,data=uswages,log='y',boxwex=0.45,col="lightgrey",main="Wages in Different Areas",ylab="wage",xlab="area",ylim=c(50,5000))
plot(exper,wage,log = 'y',main="Relationship between Wages and Experience",ylim=c(50,5000))
lines(lowess(exper,wage),col='red',lwd=1.5)

wage_ne<-vector(mode = "numeric",length=0)
wage_mw<-vector(mode = "numeric",length=0)
wage_we<-vector(mode = "numeric",length=0)
wage_so<-vector(mode = "numeric",length=0)
j<-0
k<-0
m<-0
n<-0

for(i in 1:2000){
  if(area[i]=="NE"){
    j<-j+1
    wage_ne[j]<-wage}
  else if(mw[i]=="MW"){
    k<-k+1
    wage_mw[k]<-wage[i]}
  else if(we[i]=="WE"){
    m<-m+1
    wage_we[m]<-wage[i]}
  else{
    n<-n+1
      wage_so[n]<-wage[i]}
}


wage_ne<-vector(mode = "numeric",length=0)
wage_mw<-vector(mode = "numeric",length=0)
wage_we<-vector(mode = "numeric",length=0)
wage_so<-vector(mode = "numeric",length=0)
j<-0
k<-0
m<-0
n<-0

for(i in 1:2000){
  if(area[i]=="NE"){
    j<-j+1
    wage_ne[j]<-wage[i]}
  else if(area[i]=="MW"){
    k<-k+1
    wage_mw[k]<-wage[i]}
  else if(area[i]=="WE"){
    m<-m+1
    wage_we[m]<-wage[i]}
  else{
    n<-n+1
    wage_so[n]<-wage[i]}
}

hist(wage_mw,main = "Histogram of Wages in Midwest",xlab="wages in midwest",xlim=c(0,2500),ylim=c(0,350))
hist(wage_ne,main = "Histogram of Wages in North East",xlab="wages in north east",xlim=c(0,3500),ylim=c(0,350))
hist(wage_so,main = "Histogram of Wages in South",xlab="wages in south",xlim=c(0,3500),ylim=c(0,350))
hist(wage_we,main = "Histogram of Wages in West",xlab="wages in west",xlim=c(0,3500),ylim=c(0,350))











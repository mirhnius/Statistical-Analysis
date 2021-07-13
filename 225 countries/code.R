#please write the file that r script exsits in in setwd function
setwd("C:/Users/ASUS/Desktop/project")
dataofworld<-read.csv("countries of the world.csv", header=TRUE)
na_strings <- c("NA", "N A", "N / A", "N/A", "N/ A", "Not Available", "NOt available",""," ")
dataofworlds <- readr::read_csv("countries of the world.csv", na = na_strings)
str(dataofworld)

#1
library(naniar)
#all the lengths are the same
cl.length<-length(dataofworld$Climate)
missed.Service<-sum(is.na(dataofworlds$Service)+is.null(dataofworlds$Service))
missed.climate<-sum(is.na(dataofworlds$Climate)+is.null(dataofworlds$Climate))
missed.Country<-sum(is.na(dataofworlds$Country)+is.null(dataofworlds$Country))
missed.Population<-sum(is.na(dataofworlds$Population)+is.null(dataofworlds$Population))
missed.Area<-sum(is.na(dataofworlds$`Area (sq. mi.)`)+is.null(dataofworlds$`Area (sq. mi.)`))
missed.Pop.Density<-sum(is.na(dataofworld$`Pop. Density (per sq. mi.)`)+is.null(dataofworlds$`Pop. Density (per sq. mi.)`))
missed.Coastline<-sum(is.na(dataofworlds$`Coastline (coast/area ratio)`)+is.null(dataofworlds$`Coastline (coast/area ratio)`))
missed.migration<-sum(is.na(dataofworlds$`Net migration`)+is.null(dataofworld$`Net migration`))
missed.mortality<-sum(is.na(dataofworlds$`Infant mortality (per 1000 births)`)+is.null(dataofworld$`Infant mortality (per 1000 births)`))
#the percentage of missed data
ratio.climate<-missed.climate/cl.length
ratio.Country<-missed.Country/cl.length
ratio.Population<-missed.Population/cl.length
ratio.Area<-missed.Area/cl.length
ratio.Pop.Density<-missed.Pop.Density/cl.length
ratio.Coastline<-missed.Coastline/cl.length
ratio.Coastline<-missed.Coastline/cl.length
ratio.Service<-missed.Service/cl.length
#***************************************************************
#2
boxplot(dataofworld$GDP....per.capita.,na.rm=TRUE,horizontal = TRUE,main="Box Plot",ylab="Industry",xlab="GDP per capita ")

quantile(dataofworld$GDP....per.capita.,na.rm=TRUE)
#0%   25%   50%   75%  100% 
#500  1900  5550 15700 55100 
hist(dataofworld$GDP....per.capita.,xlab="GDP per capita ")
cdf=ecdf(dataofworld$GDP....per.capita.)
plot(cdf,main="GDP per capita")
#***************************************************************
#3
sampleData<-sample(dataofworld$GDP....per.capita.,100)
meanGDP<-mean(sampleData,na.rm=TRUE)
varGDP<-var(sampleData,na.rm=TRUE)
sdGDP<-sd(sampleData,na.rm=TRUE)
# it seems that log(GDP) is more likely to  the normal distribution
qqnorm(log(sampleData))
qqline(log(sampleData),col='red')
conf.level<-.95
n=length(dataofworld$GDP....per.capita.)
z<-qt((1+conf.level)/2,df=n-1)
se<-sd(dataofworld$GDP....per.capita.,na.rm=TRUE)/sqrt(n)
ci<-z*se
confidenc.interval=c(meanGDP-ci,meanGDP+ci)
a<-t.test(sampleData,alternative="two.sided",mu=6500,conf.int=conf.level)
b<-t.test(sampleData,alternative="greater",mu=9860,conf.int=conf.level)

#***************************************************************
#4
dataofworld$Agriculture<-as.integer(dataofworld$Agriculture)
dataofworld$Crops....<-as.integer(dataofworld$Crops....)
dataofworld$Climate<-as.integer(dataofworld$Climate)
cor(dataofworld$Climate,dataofworld$Crops....,use="complete.obs",method="spearman")
cor(dataofworld$Climate,dataofworld$Crops....,use="complete.obs",method="pearson")
plot(dataofworld$Climate,dataofworld$Crops....)
abline(lm(dataofworld$Climate~dataofworld$Crops....))
qqplot(dataofworld$Climate,dataofworld$Crops....,xlab="climate",ylab="crops")

 #***************************************************************
 #5plus
plot(0:100,dpois(x=0:100,lambda =5),xlim=c(-2,100))
nor<-function(x){dnorm(x,mean=5,sd=sqrt(5))}
curve(nor,from=-4,to=100,add=TRUE)#for see the continues one
meansample<-rep(NA,5)#5 samples and thier means

plot(0:100,dpois(x=0:100,lambda =20),xlim=c(-2,100))
nor<-function(x){dnorm(x,mean=20,sd=sqrt(20))}
curve(nor,from=-4,to=100,add=TRUE)#for see the continues one
meansample<-rep(NA,5)#5 samples and thier means

plot(0:100,dpois(x=0:100,lambda =50),xlim=c(-2,100))
nor<-function(x){dnorm(x,mean=50,sd=sqrt(50))}
curve(nor,from=-4,to=100,add=TRUE)#for see the continues one
meansample<-rep(NA,5)#5 samples and thier means

plot(0:100,dpois(x=0:100,lambda =90),xlim=c(-2,100))
nor<-function(x){dnorm(x,mean=90,sd=sqrt(90))}
curve(nor,from=-4,to=100,add=TRUE)#for see the continues one
meansample<-rep(NA,5)#5 samples and thier means
for(i in 1:5)
{meansample[i]=mean(rpois(50,4))}
plot(density(meansample))
meansampleone<-rep(NA,10)#10 samples and thier means
for(i in 1:10)
{meansample1[i]=mean(rpois(50,4))}
plot(density(meansample1))
meansample2<-rep(NA,500)#10 samples and thier means
for(i in 1:500)
{meansample2[i]=mean(rpois(50,4))}
plot(density(meansample2))
meansample3<-rep(NA,5000)#500 samples and thier means
for(i in 1:5000)
{meansample3[i]=mean(rpois(50,4))}
plot(density(meansample3))

qqnorm(meansample)
qqline(meansample)

qqnorm(meansample1)
qqline(meansample1)

qqnorm(meansample2)
qqline(meansample2)

qqnorm(meansample3)
qqline(meansample3)
#***************************************************************
 #6
dataofworld$Climate<-as.integer(dataofworld$Climate)
sample2<-sample(dataofworld$Climate,200)
sample1<-sample(dataofworld$Crops....,200)
plot(sample2,sample1)
abline(lm(sample2~ sample1))
lm(sample2~ sample1)
curve( 0.003944 *x+ 3.6762108,from=150,to=200,ylab="Climate",xlab="crops")

#P2


data=read.csv(file=file.choose())
test_data=data[,12]
png("Box plot of players' ages.png")
boxplot(x=test_data,main=" Box plot of players' ages",horizontal=TRUE,xlab="age",col="green")

dev.off()
Qs=quantile(test_data)


png("histogram of ages.png")
histogram=hist(test_data,main="histogram of ages",xlab="age",col="red")
dev.off()

cdf=ecdf(test_data)

png("CDF of ages.png")
plot(cdf,main="CDF of ages",col="blue",xlab="age")
dev.off()
 
histogram$counts=cumsum(histogram$counts)
 
png("cumulative Distribution of ages.png")
plot(histogram,main="cumulative distribution of ages",col="blue",xlab="age")
dev.off()

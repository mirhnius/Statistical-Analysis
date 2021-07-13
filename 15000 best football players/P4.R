##P4
data=read.csv(file=file.choose())
data1=data[,15] #weight
data2=data[,14] #height
png("scatter plot of height & weirght.png")
plot(data1, data2, main = "scatter plot of height & weirght",xlab = "weight", ylab = "height",col="blue")
dev.off()
### calculating spearman COR
spearmanCor=cor(data1, data2, method = "spearman")

### calculating pearson COR
pearsonCor=cor(data1, data2, method = "pearson")

png("QQ-plot of weight & height.png")
qqplot(data1,data2,main="QQ-plot of weight & height",col="red",xlab="weight",ylab="height")
dev.off()
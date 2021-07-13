##P6
data<-read.csv(file=file.choose())

data_independent=data[,14] #height
data_dependent=data[,26] #drib_composure
sample_indep=sample(data_independent,200)
sample_dep=sample(data_dependent,200)

sample_indep=as.numeric(as.character(sample_indep))
sample_dep=as.numeric(as.character(sample_dep))

regression=lm(sample_dep~sample_indep)
png("Regression of height & drib composure.png")
plot(data_independent,data_dependent,col = "red",main = "Regression of height & drib composure",abline(regression),xlab = "height",ylab = "drib composure" )
dev.off()
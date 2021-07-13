##P3
data=read.csv(file=file.choose())
test_data=data[,15] #weight
test_sample=sample(test_data, size=100)

mean_sample=mean(test_sample)
var_sample=var(test_sample)

data_length=length(test_data)
sd=sqrt(var_sample)

main_var=var_sample*(length(test_data)-1)/length(test_data)
sd_main=sqrt(main_var)

png("normal QQ plot of weight sample.png")
qqnorm(test_sample,main=" normal QQ plot of weight sample",xlab="Normal distribution",ylab="Weight distribution",col="red")
dev.off()


## confidence interval
 error = qnorm(1-0.05/2)*main_var/sqrt(100)
left = mean_sample-error
right = mean_sample+error

## 2-sided p-value    claim_mean=71
claim_mean=71
z = (mean_sample-claim_mean)/(var_sample/sqrt(100))
p_value=2*pnorm(-abs(z))

## 1-sided p-value    claim_mean=71
claim_mean=71
z = (mean_sample-claim_mean)/(var_sample/sqrt(100))
p_value2=1-pnorm(-abs(z))


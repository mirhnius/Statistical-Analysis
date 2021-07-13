##P5
poisson_points=dpois(x=0:100,lambda=10)
png("PMF for a poisson distribution.png")
plot(0:100,poisson_points,main = "PMF for a poisson distribution",xlab=" X ",ylab="Probability ")
dev.off()


sample_5=rpois(5, 4)
PMF_5=dpois(x=sample_5,lambda=4)

sample_50=rpois(50, 4)
PMF_50=dpois(x=sample_50,lambda=4)

sample_5000=rpois(5000, 4)
PMF_5000=dpois(x=sample_5000,lambda=4)

png("PMF for possion sample n=5 .png")
plot(sample_5,PMF_5,main = "PMF for possion sample: n=5 ",xlab=" X ",ylab="Probability ")
dev.off()

png("PMF for possion sample n=50 .png")
plot(sample_50,PMF_50,main = "PMF for possion  sample: n=50 ",xlab=" X ",ylab="Probability ")
dev.off()

png("PMF for possion sample n=5000 .png")
plot(sample_5000,PMF_5000,main = "PMF for possion sample: n=5000 ",xlab=" X ",ylab="Probability ")
dev.off()

png("Normal QQ plot for sample n=5 .png")
qqnorm(sample_5,main="Normal QQ plot for sample: n=5",xlab="Normal distribution",ylab="possion distribution")
dev.off()

png("Normal QQ plot for sample n=50 .png")
qqnorm(sample_50,main="Normal QQ plot for sample: n=50",xlab="Normal distribution",ylab="possion distribution")
dev.off()

png("Normal QQ plot for sample n=5000 .png")
qqnorm(sample_5000,main="Normal QQ plot for sample: n=5000",xlab="Normal distribution",ylab="possion distribution")
dev.off()

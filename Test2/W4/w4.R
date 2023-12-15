# Random Sampling Distribution Simulation
nqtr<-function(x,d){noquote(t(round.object(x, d)))}

# First, set seed so we will get the same results
set.seed(145)

# Create a distribution with mean of 10 and standard deviation of 2
pop<-rnorm(n = 10000, mean = 5, sd = 1)

# Create a histogram of the population distribution
hist.grouped(pop, anchor.value = 0)
Hist.grouped(pop, anchor.value=0)

# Calculate the descriptive statistics of the population distribution
nqtr(summary.continuous(pop, stat.sd=T),5)

# Next, create / simulate a random sampling distribution
# Set the sample size equal to 4
n<-4

# Create the number of repetitions 
# (number of times we will take a sample of size 4)
reps<-5000

# Take the random samples from a population with a mean of 10 and 
# standard deviation of 2
samples <- replicate(reps, rnorm(n, mean = 10, sd = 2))
View(samples)

# Calculate the averages of each sample of 4
sampleavgs <- colMeans(samples)

# Calculate the descriptive statistics of the RSD
nqtr(summary.continuous(sampleavgs, stat.sd=T),5)

# Create 2 histograms of the population and sample averages 
# using the same axis to compare

# Create a 1 x 2 matrix
dev.off() # turns off the default
layout(matrix(1:2, nrow=2)) 

# Create both histograms
hist.grouped(pop, xlim=c(0,22), xaxt='n', width.consider = 1)
axis(side = 1, at = seq(0,22,2), labels = seq(0,22,2))

hist.grouped(sampleavgs, xlim=c(0,22), xaxt='n', width.consider = 1)
axis(side = 1, at = seq(0,22,2), labels = seq(0,22,2))





# First, set seed so we will get the same results
set.seed(100)

# Create an exponential distribution with mean of 10
popexp<-rexp(n = 50000, rate = 1/10)




# Next, create / simulate a random sampling distribution
set.seed(100)

# Set the sample size equal to 100
n<-100

# Create the number of repetitions 
# (number of times we will take a sample of size 100)
reps<-5000

# Take the random samples from an exponential population with a  
# mean of 5 (lambda = 1/5)
samplesexp <- replicate(reps, rexp(n, rate = 1/5))
#View(samplesexp)

# Calculate the averages of each sample of 100
sampleavgsexp <- colMeans(samplesexp)

# Calculate the descriptive statistics of the RSD of the means
nqtr(summary.continuous(sampleavgsexp, stat.sd=T),5)


# Create a histogram for the RSD of the means from the
# exponential population
hist.grouped(sampleavgsexp)

# Create 2 histograms of the population and sample averages 
# using the same axis to compare

# Create a 1 x 2 matrix
dev.off() # turns off the default
layout(matrix(1:2, nrow=2)) 
# Create both histograms
hist.grouped(popexp, xlim=c(0,50), xaxt='n', width.consider = 2)
axis(side = 1, at = seq(0,50,5), labels = seq(0,50,5))

hist.grouped(sampleavgsexp, xlim=c(0,50), xaxt='n', width.consider = 2)
axis(side = 1, at = seq(0,50,5), labels = seq(0,50,5))



#standard error of mean
# Using the RSD to solve probability problems
# Example 1
# Define the variables in the problem
mu1<-150
sigma1<-10
n1<-15
stderror1<-sigma1/sqrt(n1)


xbar1<-155
# Calculate the area under the normal curve using the pnorm function
pnorm(q = xbar1, mean = mu1,sd = stderror1, lower.tail = F)

# Create the normal curve
x=seq(1.28,1.45,length=200)
y=dnorm(x,mean=mu1,sd=stderror1)
plot(x,y,type="l")

# Indicate the location of the xbar of 1.433
abline(v=1.433)





#the probability of drawing a sample with a mean of


# Example 2
# Define the variables in the problem
mu2<-1.575
sigma2<-.01
n2<-5
xbar2<-1.580
stderror2<-sigma2/sqrt(n2)

# Calculate the area under the normal curve using the pnorm function
pnorm(q = xbar2, mean = mu2,sd = stderror2, lower.tail = F)

# Create the normal curve
x=seq(1.4,1.6,length=200)
y=dnorm(x,mean=mu2,sd=stderror2)
plot(x,y,type="l")

# Shade the upper tail area
x=seq(1.55,1.6,length=100)
y=dnorm(x,mean=mu2,sd=stderror2)
polygon(c(1.5,x,1.6),c(0,y,0),col="red")

# Indicate the location of the xbar of 55
abline(v=1.58)


# Example 2
# Define the variables in the problem
mu2<-1.575
sigma2<-.01
n2<-10
xbar2<-1.572
stderror2<-sigma2/sqrt(n2)

# Calculate the area under the normal curve using the pnorm function
pnorm(q = xbar2, mean = mu2,sd = stderror2, lower.tail = T)







#Confidence intervals
df=read.csv(file ="preforms.dat")

# Point Estimates
n=length(df$weight)
summary.continuous(df)

xbar<-mean(df$weight)
sd<-sd(df$weight)
vars=var(df$weight)
(prop<-mean(df$weight))

(count<-mean(Point_Estimates$Count))









# Confidence Intervals for the Mean and Variance



# Startup Code
require(lolcat)
ro<-round.object
nqtr<-function(x,d){noquote(t(round.object(x, d)))}
options(scipen=999)

# Confidence Interval for the Mean (Sigma is Known) -----------------------
# Example 1
n<-n
xbar<-xbar
sd<-sd
conf<-0.95

z.test.onesample.simple(sample.mean = xbar
                        ,known.population.variance = vars
                        ,sample.size = n
                        ,conf.level = conf)

# Round the output
z.test.onesample.simple(sample.mean = 20
                           ,known.population.variance = 5^2
                           ,sample.size = 150
                           ,conf.level = 0.95)


# Confidence Interval for the Mean (Sigma is Unknown) ---------------------
# Example 2


t.test.onesample.simple(sample.mean = xbar
                        ,sample.variance = sd^2
                        ,sample.size = n
                        ,conf.level = conf)


# Confidence Interval for the Variance ------------------------------------
# Example 3
sd<-sd
n<-n
conf<-0.95

variance.test.onesample.simple(sample.variance = vars
                                       ,sample.size = n
                                       ,conf.level = 0.90)

variance
# Confidence Interval for the Variance
ci.var$conf.int

# Confidence Interval for the Standard Deviation
sqrt(ci.var$conf.int)

# Generate Confidence Intervals using a file
# Using the point estimate file, calculate the 95% confidence interval 
# estimates for the mean, variance and standard deviation for the Weight data

# Test for normality
summary.continuous(Point_Estimates$Weight)

# Calculate confidence intervals
t.test.onesample(Point_Estimates$Weight, conf.level = 0.95)















#Generate a 95% confidence interval for the proportion nonconforming.
n=500
nonconf=15
propor=nonconf/n
conf=.95

# Confidence Interval for a Proportion -----------------------
# Example 1
(proportion.test.onesample.exact.simple(sample.proportion = propor
                                          ,sample.size = n
                                          , conf.level = 0.95))

# Proportion (Exact) 
ro(proportion.test.onesample.exact.simple(sample.proportion = 0.03525
                                          ,sample.size = 20
                                          ,conf.level = 0.90),4)



#Poisson
#The average number of indications per bar 
n=250
lambda=2.58
conf=.9
sampl_count=lambda*n
# Get Total Counts in the Sample
#(counts<-sum(Point_Estimates$Count))

# Get Sample Size
#(n<-length(Point_Estimates$Count))

poisson.test.onesample.simple(sample.count = sampl_count
                              ,sample.size = n
                              ,conf.level = 0.90)

weight <- data.frame(Values = c(6.649, 6.639, 6.629, 6.642, 6.624, 6.616, 6.649, 6.654, 
                            6.649, 6.642, 6.649, 6.637, 6.649, 6.647, 6.644, 6.657, 
                            6.654, 6.634, 6.644, 6.649))
preform<-data.frame(weight)
# Calculate the mean of the ungrouped data
mean(preform$weight)

# Calculate the mean from grouped data
# Assign the output of the grouped frequency distribution
# to a variable called "fdcast"
(fdcast<-frequency.dist.grouped(castings$weight))

# Look at the structure of fdcast
str(fdcast)

# Create a vector of the midpoints of each class
# interval in the frequency distribution
# (put in parentheses to view output)
(midpts<-fdcast$midpoint)

# Create a vector of the frequency of each class
# interval in the frequency distribution
# (put in parentheses to view output)
(freq<-fdcast$freq)

# Use the weighted mean command in the base package
# to calculate the  mean of the grouped data
weighted.mean(x = midpts, w = freq)

# For comparison, calculate the ungrouped mean of the 
# Casting Weight data
mean(castings$weight)

# Weighted Mean
wt<-c(0.2, 0.4, 0.4)
x<-c(88, 85, 92)
weighted.mean(x = x, w = wt)

# Median
median(preform$weight)

# Mode 
table(preform$weight)
sample.mode(preform$weight)


# What if there are 2 modes? Add in another 57
weight<-c(65,67,36,37,36,57,53,39,38,58,57)
preform<-data.frame(weight)
View(preform)

# Calculate the Mode
sample.mode(preform$weight)






#quantile

# Create data file


# Find the range of values in the data set
range(preform$weight)
rng<-range(preform$weight)
rng[2]-rng[1]

# Find the Interquartile Range of the data set
IQR(preform$Values)

# Find the standard deviation of the data set
sd(preform$weight)
round.object(sd(preform$weight),2)

# Find the variance of the data set
var(preform$weight)
round.object(var(preform$weight),2)

quantile(preform$Values)




##shape skew

# Calculate skewness for the castings data
skewness(castings$weight)
round.object(skewness(castings$weight),3)

# Calculate kurtosis for the castings data
kurtosis(castings$weight)
round.object(kurtosis(castings$weight),3)

# Calculate descriptive statistics using the summary.continuous command
summary.continuous(castings$weight, stat.sd=T)
round.object(summary.continuous(castings$weight, stat.sd=T),3)

# Transpose the output and remove the quotes
t(round.object(summary.continuous(castings$weight, stat.sd=T),3))
noquote(t(round.object(summary.continuous(castings$weight, stat.sd=T),3)))

# Create a function to round, transpose and remove the quotes
nqtr<-function(x,d){noquote(t(round.object(x, d)))}
nqtr(summary.continuous(castings$weight),3)












# Transform castings3 data from independent to dependent format
castnew<-transform.independent.format.to.dependent.format(fx = weight~mold, data = castings3)

# View current column names
colnames(castnew)

# Rename column headings
colnames(castnew)[1:3] <-c("Mold_1","Mold_2", "Mold_3")

# View new column heading names
View(castnew)

# Calculate Pearson Product-Moment Correlation Coefficient
cor(x = castnew$Mold_1, y = castnew$Mold_2, method = "pearson")

# Create scatterplot
plot(x = castnew$Mold_1, y =castnew$Mold_2,pch=19, cex=1, xlab="Mold 2", ylab="Mold 1")
abline(lm(castnew$Mold_2~castnew$Mold_1),col="blue",lwd=2)


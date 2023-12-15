library(lolcat)

df=read.csv("Desktop/Test2/viscosity.txt")
hist.ungrouped(df$centistokes)

hist.grouped(df$centistokes)
boxplot(df$centistokes)
summary(df)
spc.run.chart(chart.series =df$centistokes, main = "Run Chart: Computer Fans"
              ,ylab = "CFM",pch = 19, cex=1.2, col="blue", lty=1, lwd=2
              ,type = "o")


#Ungrouped Frequency Distribution
frequency.dist.ungrouped(df$centistokes)

#Grouped Frequency Distribution
frequency.dist.grouped(df$centistokes)

range(df$centistokes)
summary(df$centistokes)
summary.continuous(df$centistokes)
frequency.dist.ungrouped(df)
IQR(df$centistokes)

quantile(df$centistokes,probs = .45)
summary(df$centistokes)




df2=read.csv("Desktop/Test2/yield.txt")
hist.ungrouped(df2)

hist.grouped(df2$strength,freq = F)
boxplot(df2)
summary(df2)

#Hist Density etc
hist.grouped(df2$strength, xlab="Weight", freq=F)
lines(density(df2$strength))

plot(density(df2$strength), main="Density Plot of Casting Weight", xlab="Weight")

dp<-density(df2$strength)
plot(dp, main="Density Plot of Casting Weight", xlab="Weight")
polygon(dp, col="red", border="black")

skewness(df2$strength)
skewness.test(df2$strength)
kurtosis(df2$strength)

#run chart
spc.run.chart(chart.series =df$strength, main = "Run Chart: Computer Fans"
              ,ylab = "CFM",pch = 19, cex=1.2, col="blue", lty=1, lwd=2
              ,type = "o")


df3=read.delim("Desktop/Test2/body.txt",)
hist.grouped(df3$height)

castnew<-transform.independent.format.to.dependent.format(fx = height~weight, data = df3)

# View current column names
colnames(castnew)

# Rename column headings
colnames(castnew)[1:2] <-c("Mold_1","Mold_2")

# View new column heading names
View(castnew)

# Calculate Pearson Product-Moment Correlation Coefficient
cor(x = df3$height, y = df3$weight, method = "pearson")


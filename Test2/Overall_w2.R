

castings=read.csv('Desktop/Test2/castings.txt')

castings3=read.delim('Desktop/Test2/castings3.txt')

library(lolcat)


#Hist Density etc
hist.grouped(castings$weight, xlab="Weight", freq=F)
lines(density(castings$weight))

plot(density(castings$weight), main="Density Plot of Casting Weight", xlab="Weight")

dp<-density(castings$weight)
plot(dp, main="Density Plot of Casting Weight", xlab="Weight")
polygon(dp, col="red", border="black")

#frequency
frequency.polygon.grouped(castings$weight, main="Grouped Frequency Polygon: Casting Weight",xlab="Casting Weight")
hist.ungrouped(castings$weight, main="Ungrouped Histogram",xlab="CFM")
hist.grouped(castings$weight, main="Grouped Histogram: Castings",xlab="Weight")


#box whiskers
summary(castings$weight)

boxplot(castings$weight, main="Boxplot of Casting Weight", ylab="Weight")

boxplot(castings$weight, main="Boxplot of Casting Weight", ylab="Weight", notch=T)

boxplot(weight ~ mold, data = castings3, main="Boxplot of Casting Weight by Mold", ylab="Weight") 


crack=read.delim(file = "returns.dat")


p1=mean(crack$OldSite)
p2=mean(crack$YourSite)
length(crack$OldSite)

var1=var(crack$OldSite)
var2=var(crack$YourSite)


line1=subset(crack, OldSite==0)
line2=subset(crack,OldSite==0)
hist(line1$Result)
hist(line2$Result)
line1$Result

proportion.test.twosample.exact.simple(sample.proportion.g1 = p1,sample.size.g1 = 1000,
                                       sample.proportion.g2 =p2,sample.size.g2 = 1000,conf.level = .95 )

t.test.twosample.independent.simple(sample.mean.g1 = p1,sample.variance.g1 = var1,sample.size.g1 = 1000,
                                    sample.mean.g2 = p2,sample.variance.g2 = var2,sample.size.g2 = 1000,conf.level = .9)                                       

t.tes                         
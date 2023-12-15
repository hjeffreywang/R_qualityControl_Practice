#what would the power be for to detect a change in the Mean?
alpha= 0.01
n=20
deltamu=1
sigmas=2

power.mean.t.onesample(effect.size = 1,alpha = alpha,variance.est = sigmas^2,sample.size = n)



#what would be the minimum sample size needed to to detect a change in the Mean? 
alpha= 0.01
beta=.05

deltamu=1
sigmas=2
sigmas2=1
sample.size.mean.t.onesample(effect.size=deltamu,alpha = alpha,beta = beta, variance.est= sigmas^2,alternative = "two.sided")




#What is the value of the test statistic
#What is the p value for the test statistic?

t.test.twosample.independent.simple(sample.mean.g1 = 72
                                    ,sample.variance.g1 = 15^2
                                    ,sample.size.g1 = 12
                                    ,sample.mean.g2 = 64
                                    ,sample.variance.g2 = 19^2
                                    ,sample.size.g2 = 15
                                    ,conf.level = 0.95)

#p value>alpha so not enough evidence to conclude there is a significant difference


#SIGNIFICANCE OF CHANGE IN VARIABILITY CHANGE  F STAT
variance.test.twosample.independent.simple(,sample.variance.g1 = 15^2
                                           ,sample.size.g1 = 12
                                           ,sample.variance.g2 = 19^2
                                           ,sample.size.g2 = 15
                                           ,conf.level = 0.95)



#What is the value of the proper test statistic?
#Which Vendor has a higher variance
toollife=read.delim(file = "ToolLife.dat")
toollife$vendor

vendor1_life <- subset(toollife, vendor == "1")
vendor2_life <- subset(toollife, vendor == "2")

mean(vendor1_life$life)
mean(vendor2_life$life)

var(vendor1_life$life)
var(vendor2_life$life)


t.test.twosample.independent()


#DEPENDENT 
#Has the straightening operation significantly changed the process mean? 
straight=read.delim(file = "Straight.dat")

straight$diff=straight$before- straight$after
meandiff=mean(straight$diff)
varsdiff=var(straight$diff)

t.test.twosample.dependent(x1 = straight$before,x2 = straight$after,conf.level = .9)


#t.test.twosample.dependent.simple.dbar(pair.differences.mean = straight$after - str)
#t.test.twosample.independent()
t.test.twosample.dependent.simple.dbar(pair.differences.mean = meandiff
                                       ,pair.differences.variance = varsdiff
                                       ,sample.size = length(straight$before))

#dbar differences between x y into another column z (noise$diff)
## Dbar method       t.test.twosample.dependent.simple.dbar(pair.differences.mean = mean(Noise$Diff)#
#                                       ,pair.differences.variance = var(Noise$Diff)
#                                       ,sample.size = 10)


#MATCHED PAIRS DEP
# Test an appropriate hypothesis using an alpha = 0.05. Matched pairs
#What is the value of the proper test statistic?
Mean1 = 3671 
s1 = 246 
n1 = 50

Mean2 = 4228 
s2 = 182
n2 = 50
r12 = 0.78

cor.pearson.r.onesample.simple(sample.r = ,sample.size = n,null.hypothesis.rho =  )

t.test.twosample.dependent.simple.meandiff(sample.mean.g1 = Mean1
                                           ,sample.variance.g1 = s1^2
                                           ,sample.size= n1
                                           ,sample.mean.g2 = Mean2
                                           ,sample.variance.g2 = s2^2
                                           ,conf.level = 0.95,rho.estimate = r12)

#====================================================================================


temper=read.delim(file = "Temper.dat")

#What is the value of the test statistic for the test of variance? 
variance.test.twosample.dependent(g1 =temper$before ,
                                  g2= temper$after,conf.level = .95)

"mean.z.test.twosample.independent.simple(sample.mean.g1 = mean(temper$before),
                                         sample.mean.g2 = mean(temper$after),
                                         variance.g1 =var(temper$before),
                                         variance.g2 = var(temper$after),sample.size.g1 = 20,sample.size.g2 = 20,conf.level = .95)"


#what is the value of the test statistic for the test of means? 
t.test.twosample.dependent(x1=temper$before, x2=temper$after, conf.level = .95 )

"t.test.twosample.dependent.simple.meandiff(sample.mean.g1 = mean(temper$before),
                                           sample.mean.g2 = mean(temper$after),
                                           sample.variance.g1 = var(temper$before),
                                           sample.variance.g2 = var(temper$after),
                                           sample.size =length(temper$before),conf.level = .95 
                                          )"
#iso
plot(x=temper$before, y=temper$after)

#What is the value of the test statistic for the test of means? 

d
#True or False: The line of best fit and ISO line are parallel.


#Has an improvement been made with respect to the mean level of warp after tempering?
#p <alpha so yes, impact made

######################################################################

#POISSON COUNT two indep process ssamples drawn


###FISHERS EXACT
#if the nonconforming rate of all combined visual characteristics is different for the two machines. 
p1  = 0.054
n1 =  500
p2 = 0.036
n2 = 500

proportion.test.twosample.exact.simple(sample.proportion.g1 = p1,sample.size.g1 = n1,
                                       sample.proportion.g2 =p2,sample.size.g2 = n2,conf.level = .95 
                                      )


#poisson.test.twosample.simple(sample.count.g1 = p1*n1,sample.size.g1 = n1,sample.count.g2 = p2*n2,sample.size.g2 = n2,conf.level = .95)
##################################################
#proportions
#Fisher's test for pass fail
reje_before=14
acc_before=102+34
reje_aft=10+34
acc_aft=102

# McNemar's Test for Change - Dependent Proportions -----------------------
# Contingency table format = ct<-(a,c,b,d)
ct<-c(102,34,4,10)

# Create Contingency Table
(ct.new<-matrix(ct,nrow = 2
                , dimnames = list("Before Maint" = c("Pass", "Fail"),
                                  "After Maint" = c("Pass", "Fail"))))

# Perform McNemar's Test
(mcnemar.out<-proportion.test.mcnemar.simple(b = 4, c = 56))

mcnemar.test(ct.new)


  
  
  
#proportion failures improved over the year
#poisson

poisson.test(x = c(15,10),T = c(2,1),conf.level = .95,alternative = 'less')
poisson.test()
poisson.test.twosample.simple(sample.count.g1 = 15,sample.size.g1 = 2,
                              sample.count.g2 = 10,sample.size.g2 = 1,conf.level = .95)
poisson.test.twosample.simple()
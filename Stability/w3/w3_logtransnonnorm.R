require((lolcat))
Bagofweights <- read.csv("BagWeightsDev.txt", sep="")
Bagofweights$Deviation




# Summary and Histogram

nqtr(summary.continuous(Bagofweights$Deviation), 5)
summary(Bagofweights$Deviation)
hist.grouped(Bagofweights$Deviation, anchor.value = -2)
hist.grouped(Bagofweights$Deviation, xlim=c(min(Bagofweights$Deviation),max(Bagofweights$Deviation)), anchor.value = min(Bagofweights$Deviation),main = "Original Temperature Data")
hist.add.distribution.curve.normal(x = Bagofweights$Deviation)

spc.individuals.chart(Bagofweights$Deviation)

# When data are slightly positively skewed, and kurtosis
# is ok, the lognormal transformation may be used


###########################################################
Bagofweights$Deviation<-log(2*abs(min(Bagofweights$Deviation)) + Bagofweights$Deviation)


# Check transformed data for normality
nqtr(summary.continuous(Bagofweights$Deviation),5)
hist.grouped(Bagofweights$Deviation, anchor.value = 3.61, interval.size = 0.03, freq = F)
hist.add.distribution.curve.normal(x = Bagofweights$Deviation, freq = F)

# Find the Upper Natural Process Limit and 
# Lower Natural Process Limit on the transformed data
natural.tolerance.normal(x = Bagofweights$Deviation)

Delivery2.ln<-natural.tolerance.normal(x = Bagofweights$Deviation)
(LNPL.ln<-Delivery2.ln$lower.limit)
(UNPL.ln<-Delivery2.ln$upper.limit)

# Transform UNPL and LNPL back to original data
(LNPL<-exp(LNPL.ln-2-abs(min(Bagofweights$Deviation))))
(UNPL<-exp(UNPL.ln-2-abs(min(Bagofweights$Deviation))))

# Overall natural tolerance
UNPL-LNPL

# Create X and moving range chart with new limits for X chart
spc.chart.variables.individual.and.movingrange.generic.simple(individuals = Bagofweights$Deviation
                                                              ,chart1.center.line = median(Bagofweights$Deviation)
                                                              ,chart1.control.limits.lcl = LNPL
                                                              ,chart1.control.limits.ucl = UNPL)


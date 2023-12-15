# Startup Code
require(lolcat)
xbar.bagwt <- read.delim("Bagweight.txt")
#xbar.r <- read.delim("xbar-r.dat")

mean(xbar.bagwt$Bag_Weight)
# Drop first column
#xbar.r <- xbar.r[-c(1)]

# Transpose Data ----------------------------------------------------------
xbar.bagwt.new<-xbar.bagwt

#####################
# Create a sample column
xbar.bagwt.new$sample<-rep(1:30) 

# Drop the week column
xbar.bagwt.new<-xbar.bagwt.new[-1]

# Sort by sample
xbar.bagwt.column<-xbar.bagwt.new[order(xbar.bagwt.new$sample),]
#xbar.bagwt.column

##########


# Create the X Bar and R Chart --------------------------------------------
res=spc.chart.variables.mean.and.meanrange(data = xbar.bagwt.column$Bag_Weight
                                           ,sample = xbar.bagwt.column$sample
                                           ,chart1.main="Xbar Chart"
                                           ,chart2.main="R Chart")

# Extract Control Limits and Centerline for each chart --------------------
xbar.bagwt.eval<-spc.chart.variables.mean.and.meanrange(data = xbar.bagwt.column$Bag_Weight
                                                        ,sample = xbar.bagwt.column$sample)
xbar.bagwt.eval$chart2.center.line

lims.chart.1 <- unique(data.frame(
  lcl     = xbar.bagwt.eval$chart1.control.limits.lcl
  ,center = xbar.bagwt.eval$chart1.center.line
  ,ucl    = xbar.bagwt.eval$chart1.control.limits.ucl
))

lims.chart.1
lims.chart.2 <- unique(data.frame(
  lcl     = xbar.bagwt.eval$chart2.control.limits.lcl
  ,center = xbar.bagwt.eval$chart2.center.line
  ,ucl    = xbar.bagwt.eval$chart2.control.limits.ucl
))




xbar.bagwt.eval$chart2.center.line


###########################################


# Evaluate Chart for Control ----------------------------------------------
xbar.bagwt.evalctrl<-spc.chart.variables.mean.and.meanrange(data = xbar.bagwt.column$Bag_Weight
                                                    ,sample = xbar.bagwt.column$sample
                                                    ,chart1.control.rules = spc.rulesets.nelson.1984.test.1.2.3.4.5.6.7.8()
                                                    ,chart2.control.rules = spc.rulesets.nelson.1984.test.1.2.3.4())
xbar.r.eval$chart2.control.limits.lcl

lims.chart.2 <- unique(data.frame(
  lcl     = xbar.bagwt.evalctrl$chart2.control.limits.lcl
  ,center = xbar.bagwt.evalctrl$chart2.center.line
  ,ucl    = xbar.bagwt.evalctrl$chart2.control.limits.ucl
))




"
any(xbar.r.eval$chart1.is.control.violation$overall.results)
any(xbar.r.eval$chart2.is.control.violation$overall.results)

# Determine which rules were violated -------------------------------------
lapply(xbar.r.eval$chart1.is.control.violation$rule.results
       ,FUN = function(v) { any(v) } )
lapply(xbar.r.eval$chart2.is.control.violation$rule.results
       ,FUN = function(v) { any(v) } )

# Determine which points are out of control -------------------------------
lapply(xbar.r.eval$chart1.is.control.violation$rule.results
       ,FUN = function(v) { which(v) } )
lapply(xbar.r.eval$chart2.is.control.violation$rule.results
       ,FUN = function(v) { which(v) } )"

# Estimate the standard deviation from the control chart ------------------
(sig.est<-unique(xbar.bagwt.evalctrl$chart2.center.line)/spc.constant.calculation.d2(5))



spc.chart.variables.mean.and.meanrange(data = xbar.bagwt.column$Bag_Weight
                                       ,sample = xbar.bagwt.column$sample)


#########################################################################################
=
  
  
  
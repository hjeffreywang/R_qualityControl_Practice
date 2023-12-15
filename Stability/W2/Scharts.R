sheets= read.delim("NKgE2MCPS52oBNjAjyudNA_7ccee294a9334085af6a58bf5e4d16f1_ShearedSheet.txt")

#sheets.new<-transform.independent.format.to.dependent.format(data = sheets,fx = "Subgroup")
sheets.new=sheets
# Create a sample column
sheets.new$sample<-rep(1:8)

sheets.new<-sheets.new[-1]
sheets.column<-sheets.new[order(sheets.new$sample),]

spc.chart.variables.mean.and.meanstandarddeviation(data =sheets.column$Width_Deviation
                                                   , sample = sheets.column$sample
                                                   , chart1.main="Mean Chart"
                                                   , chart2.main="S Chart"
                                                   , chart1.ylab="Amps"
                                                   , chart2.ylab="Std. Dev.")
evalss=spc.chart.variables.mean.and.meanstandarddeviation(data =sheets.column$Width_Deviation
                                                          , sample = sheets.column$sample
                                                          , chart1.main="Mean Chart"
                                                          , chart2.main="S Chart"
                                                          , chart1.ylab="Amps"
                                                          , chart2.ylab="Std. Dev.")
evalss$chart1.center.line
evalss$chart2.center.line
evalss$chart2.control.limits.ucl

evalssctrl=spc.chart.variables.mean.and.meanstandarddeviation(data =sheets.column$Width_Deviation
                                                          , sample = sheets.column$sample
                                                          , chart1.main="Mean Chart"
                                                          , chart2.main="S Chart"
                                                          , chart1.ylab="Amps"
                                                          , chart2.ylab="Std. Dev."
                                                          , chart1.control.rules = spc.rulesets.nelson.1984.test.1.2.3.4.5.6.7.8())


evalssctrl$chart1.control.limits.ucl
evalssctrl$chart1.control.limits.lcl
evalssctrl$chart2.control.limits.ucl

evalssctrl$chart2.control.limits.lcl


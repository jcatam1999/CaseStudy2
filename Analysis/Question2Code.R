##Set Working directory
setwd('C:\\RProjects\\CaseStudy2\\Analysis')

##Install tseries package
source('Packageinstall.R')

##get the data
histquot = get.hist.quote('agio', quote = 'Close')

##Get return
snpreturn = log(lag(histquot)) - log(histquot)

##volatility
snpvol = sd(snpreturn) * sqrt(250) * 100
##Function
vol = function(d, logrets){
  var=0
  lam=0
  varlist = c()
  for (r  in logrets) 
  {
    lam = lam*(1 -1/d) + 1
    var = (1-1/lam)*var + (1/lam)*r^2
    varlist = c(varlist, var)
    
  }
  sqrt(varlist)
}
volest = vol(10,snpreturn)
volest1 = vol(30, snpreturn)
volest2 = vol(100, snpreturn)
head(volest)
head(volest1)
head(volest2)
plot(volest*100, type = 'l', col='red')
lines(volest1*100, type = 'l',col='green')
lines(volest2*100, type = 'l', col='blue')
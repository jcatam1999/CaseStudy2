##Check for a required package and load if required to Add library
##install.packages('tseries')
##library(tseries)

is.installed <- function(mypkg){
  is.element(mypkg, installed.packages()[,1])
} 


if(!is.installed('tseries')){
  install.packages('tseries')
  library(tseries)
}else{
  library(tseries)
  print('Package is already installed')
}
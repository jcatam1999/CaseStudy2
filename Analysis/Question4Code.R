CityTempFile = 'C:\\RProjects\\CaseStudy2\\Analysis\\Data\\citytemp.csv'
WorldTempFile = 'C:\\RProjects\\CaseStudy2\\Analysis\\Data\\TEMP.csv'
##read the file
WorldTemp = read.csv(WorldTempFile, sep = ',', header = TRUE)

##clean up the file
##remove any empty average temps
##rename columns
##make dates a date
##make numbers a number
##function for dates
standarDates <- function(string) {
  patterns = c('[0-9][0-9][0-9][0-9]/[0-9][0-9]/[0-9][0-9]','[0-9][0-9]/[0-9][0-9]/[0-9][0-9][0-9][0-9]','[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]'
               ,'[0-9]/[0-9][0-9]/[0-9][0-9][0-9][0-9]','[0-9]/[0-9]/[0-9][0-9][0-9][0-9]','[0-9][0-9]/[0-9]/[0-9][0-9][0-9][0-9]')
  formatdates = c('%Y/%m/%d','%d/%m/%Y','%Y-%m-%d','%m/%d/%Y','%m/%d/%Y','%m/%d/%Y')
  standardformat='%d/%m/%Y'
  for(i in 1:6){
    if(grepl(patterns[i], string)){
      aux=as.Date(string,format=formatdates[i])
      if(!is.na(aux)){
        return(format(aux, standardformat))
      }
    }
  }
  return('FALSE')
}


##conver the date
##WorldTemp$Date = as.Date(WorldTemp$Date, format='%y/%m/%d')

for(Date in WorldTemp)
  {
    print(Date)
    result = standarDates(Date)
    print(result)
}
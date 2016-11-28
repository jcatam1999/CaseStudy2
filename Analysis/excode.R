
GetYear = function(newdate){
  if(grepl('-',newdate))
  {
    #print('there is a dash')
    ldate = unlist(strsplit(newdate,'-',fixed = FALSE))
    if(nchar(as.numeric(ldate[1]))==4){
      year = as.numeric(ldate[1])
     
      
    }else{
      year = as.numeric(ldate[3])
      
    }
    
  }
  if(grepl('/',newdate)){
    #print('there is a forward slash')
    ldate = unlist(strsplit(newdate,'/',fixed = FALSE))
    
    if(nchar(as.numeric(ldate[1]))==4){
      year = as.numeric(ldate[1])
      
      
    }else{
      year = as.numeric(ldate[3])
      
    }
  }
  return(year)
}

v = GetYear('1832-1-10')

subworld = subset(WorldTemp, GetYear(WorldTemp$Date)> 1899)



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
      print('this is inside function loop')
      print(string)
      aux=as.Date(string,format=formatdates[i])
      print(aux)
      if(!is.na(aux)){
        return(format(aux, standardformat))
      }
    }
  }
  return('FALSE')
}


##conver the date
##WorldTemp$Date = as.Date(WorldTemp$Date, format='%y/%m/%d')

for(i in 1:nrow(WorldTemp))
{
  
  if(grepl('-',WorldTemp[i,1]))
  {
    #print('there is a dash')
    ldate = unlist(strsplit(WorldTemp[i,1],'-',fixed = FALSE))
    if(nchar(as.numeric(ldate[1]))==4){
      year = as.numeric(ldate[1])
      day = as.numeric(ldate[2])
      month = as.numeric(ldate[3])
      
    }else{
      year = as.numeric(ldate[3])
      day = as.numeric(ldate[2])
      month = as.numeric(ldate[1])
    }
    
  }
  if(grepl('/',WorldTemp[i,1])){
    #print('there is a forward slash')
    ldate = unlist(strsplit(WorldTemp[i,1],'/',fixed = FALSE))
    
    if(nchar(as.numeric(ldate[1]))==4){
      year = as.numeric(ldate[1])
      day = as.numeric(ldate[2])
      month = as.numeric(ldate[3])
      
    }else{
      year = as.numeric(ldate[3])
      day = as.numeric(ldate[2])
      month = as.numeric(ldate[1])
    }
  }
  print(i)
  
  WorldTemp$Year[i] = year
  WorldTemp$Day[i] = day
  WorldTemp$month[i] = month
  
}

GetYear = function(x){}

gdp = subset(gdp, gdp$X!='')
subworld = subset(WorldTemp, )


WorldTemp$Year = as.numeric(unlist(str_split_fixed(WorldTemp$Date, '-', 3))[1])
WorldTemp$Month = as.numeric(unlist(str_split_fixed(WorldTemp$Date, '-', 3))[2])
head(WorldTemp)

WorldTemp$Year =  as.numeric(unlist(str_split_fixed(WorldTemp$Date, '-', 3), recursive = FALSE)[,1])
WorldTemp$Month = unlist(str_split_fixed(WorldTemp$Date, '-', 3), recursive = FALSE)[,2]

WorldTemp$Year =  as.numeric(unlist(str_split_fixed(WorldTemp$Date, '/', 3), recursive = FALSE)[,1])
WorldTemp$Month = unlist(str_split_fixed(WorldTemp$Date, '/', 3), recursive = FALSE)[,2]
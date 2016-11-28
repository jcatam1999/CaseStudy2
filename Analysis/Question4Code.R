CityTempFile = 'C:\\RProjects\\CaseStudy2\\Analysis\\Data\\citytemp.csv'
WorldTempFile = 'C:\\RProjects\\CaseStudy2\\Analysis\\Data\\TEMP.csv'
##read the file
WorldTemp = read.csv(WorldTempFile, sep = ',', header = TRUE, stringsAsFactors = FALSE)
library(stringr)
library(plyr)


#get rid of any dates less than year 1900
WorldTemp$Year =  ifelse(grepl('-',WorldTemp$Date), unlist(str_split_fixed(WorldTemp$Date, '-', 3), recursive = FALSE)[,1],'2')
WorldTemp$Year = as.numeric(WorldTemp$Year)
WorldTemp = subset(WorldTemp, (WorldTemp$Year >1899) | (WorldTemp$Year == 2))
##check the next format of dates
WorldTemp$Year =  ifelse(grepl('/',WorldTemp$Date), unlist(str_split_fixed(WorldTemp$Date, '/', 3), recursive = FALSE)[,3],'2')
WorldTemp$Year = as.numeric(WorldTemp$Year)
WorldTemp = subset(WorldTemp, (WorldTemp$Year >1899) | (WorldTemp$Year == 2))
##break out month
WorldTemp$Month =  ifelse(grepl('/',WorldTemp$Date), unlist(str_split_fixed(WorldTemp$Date, '/', 3), recursive = FALSE)[,1],'2')
##convert temp
WorldTemp$Monthly.AverageTemp = as.numeric(WorldTemp$Monthly.AverageTemp)
##remove NA
WorldTemp = subset(WorldTemp, WorldTemp$Monthly.AverageTemp !='NA')

#get mean by month
v = tapply(WorldTemp$Monthly.AverageTemp, by=c('WorldTemp$Month', 'WorldTemp$Country'), mean)
grcol = c('WorldTemp$Month', 'WorldTemp$Country')
v= aggregate(WorldTemp$Monthly.AverageTemp, by=list(WorldTemp$Country, WorldTemp$Month), mean)
v = arrange(v, v$Group.1)
head(v)




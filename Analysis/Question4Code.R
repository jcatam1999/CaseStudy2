CityTempFile = 'C:\\RProjects\\CaseStudy2\\Analysis\\Data\\citytemp.csv'
WorldTempFile = 'C:\\RProjects\\CaseStudy2\\Analysis\\Data\\TEMP.csv'
##read the file
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
#v = tapply(WorldTemp$Monthly.AverageTemp, by=c('WorldTemp$Month', 'WorldTemp$Country'), mean)
v = aggregate(WorldTemp$Monthly.AverageTemp, by = list(WorldTemp$Month, WorldTemp$Country), FUN=mean, na.rm=TRUE)
xmin = round(tapply(v$x, v$Group.2, min), digits = 2)
xmax = round(tapply(v$x, v$Group.2, max), digits = 2)

##merge the data
cminmax = data.frame(xmin,xmax)
##convert row names
cminmax = cbind(country = rownames(cminmax), cminmax)
##calculate the difference
cminmax$diff = cminmax$xmax - cminmax$xmin
##order by diff
cminmax = cminmax[order(cminmax$diff, decreasing = TRUE),]
##get top 20
top20 = cminmax[1:20,]

##plot results
#plot(top20$diff, col='blue', x=top20$country)

par(las=2)
par(mar=c(8,4,2,2))
barplot(top20$diff, main = 'Largest Tempature Difference', xlab = 'Country', ylab = 'Degree (F)')




##pull in City data
CityTemp = read.csv(CityTempFile, sep = ',', header = TRUE, stringsAsFactors = FALSE)


#get rid of any dates less than year 1900
CityTemp$Year =  ifelse(grepl('-',CityTemp$Date), unlist(str_split_fixed(CityTemp$Date, '-', 3), recursive = FALSE)[,1],'2')

##set year to numberic
CityTemp$Year = as.numeric(CityTemp$Year)

##subset the data
CityTemp = subset(CityTemp, (CityTemp$Year >1989) | (CityTemp$Year == 2))

##check the next format of dates
CityTemp$Year =  ifelse(grepl('/',CityTemp$Date), unlist(str_split_fixed(CityTemp$Date, '/', 3), recursive = FALSE)[,3],'2')
##change to number
CityTemp$Year = as.numeric(CityTemp$Year)
##subset based on year
CityTemp = subset(CityTemp, (CityTemp$Year >1989) | (CityTemp$Year == 2))
##break out month
CityTemp$Month =  ifelse(grepl('/',CityTemp$Date), unlist(str_split_fixed(CityTemp$Date, '/', 3), recursive = FALSE)[,1],'2')
##convert temp
CityTemp$Monthly.AverageTemp = as.numeric(CityTemp$Monthly.AverageTemp)
##remove NA
CityTemp = subset(CityTemp, CityTemp$Monthly.AverageTemp !='NA')

CityTemp$TempInFah = round(CityTemp$Monthly.AverageTemp *(9/5) + 32, digits = 2)

##get the average temp by year
v = round(tapply(CityTemp$TempInFah, CityTemp$Year, mean), digits = 2)
v = data.frame(v)

##convert row lable to column
v = cbind(Year = rownames(v), v)
##plot it

plot(v, xlab = "YEAR", ylab = 'Tempature (F)', col='red')
#remove row names
colnames(v) = c('year', 'temp')
#drop the levels


for (i in 1:nrow(v$temp)) {
  if(i > 1){
    print(v$temp[i] - v$temp[i-1], row.names = FALSE)
  }
}

##this section showscity temp
CityTemp$Month = as.numeric(CityTemp$Month)


v2 = aggregate(CityTemp$TempInFah, by = list(CityTemp$Month, CityTemp$City), FUN=mean, na.rm=TRUE)
xmin = round(tapply(v2$x, v2$Group.2, min), digits = 2)
xmax = round(tapply(v2$x, v2$Group.2, max), digits = 2)


##merge the data
cminmax = data.frame(xmin,xmax)
##convert row names
cminmax = cbind(country = rownames(cminmax), cminmax)
##calculate the difference
cminmax$diff = cminmax$xmax - cminmax$xmin
##order by diff
cminmax = cminmax[order(cminmax$diff, decreasing = TRUE),]
##get top 20
top20 = cminmax[1:20,]

##plot results
par(las=2)
par(mar=c(8,4,2,2))
barplot(top20$diff, main = 'Largest Tempature Difference', xlab = 'City', ylab = 'Degree (F)')


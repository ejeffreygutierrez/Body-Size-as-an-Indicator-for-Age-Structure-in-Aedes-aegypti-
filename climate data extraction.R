install.packages("devtools")
library("devtools")
install_github("ndphillips/yarrr")
library(ggplot2)
library(yarrr)

##Importing data frames. Main is the main data file, and tempsummary provides
##the date ranges and places to stick the averages###
main = read.csv(file = "Experiment_Data.csv")
tempsummary = read.csv(file = "dev temp.csv")

###Fixing the decimal place problems with the NOAA Data
main$TMAX[main$TMAX > 50] = (main$TMAX[main$TMAX > 50])/10
main$TMIN[main$TMIN > 50] = (main$TMIN[main$TMIN > 50])/10
main$DTR = (main$TMAX - main$TMIN)

#Getting rid of any extra (Nogales) Weather Stations
main = subset(main, main$StationName != "NOGALES 6 N AZ US")

##Changing date data to POSITX data for easier referencing
tempsummary$Median.Collection.Date = format(strptime(tempsummary$Median.Collection.Date,  format ="%m/%d/%y"), "%Y%m%d")
tempsummary$Dev..Date.Range.Start = format(strptime(tempsummary$Dev..Date.Range.Start, format = "%m/%d/%y"), "%Y%m%d")
tempsummary$Dev..Date.Range.End = format(strptime(tempsummary$Dev..Date.Range.End, format = "%m/%d/%y"), "%Y%m%d")
tempsummary$range.start.Avg.Temp.Prev.1Wk = format(strptime(tempsummary$range.start.Avg.Temp.Prev.1Wk, format = "%m/%d/%y"), "%Y%m%d")
tempsummary$range.start.Avg.DTR.Prev.2Wks = format(strptime(tempsummary$range.start.Avg.DTR.Prev.2Wks, format = "%m/%d/%y"), "%Y%m%d")

##The loop. It creates two vectors that match the data from tempsummary. It loops through each row of TempSummary
###and averages the temp and DTR for the date range and city provided by TempSummary.
newdat1 = c()
newdat2 = c()
newdat3 = c()
newdat4 = c()
for(i in 1:81){
  daterange = as.numeric(as.vector(tempsummary[i,6:7]))
  city = tempsummary[i,1]
  sub = subset(main, main$DATE >= daterange[1] & main$DATE <= daterange[2] & main$City == city)
  newdat1[i] = mean(sub$TempAvgC)
  newdat2[i] = mean(sub$DTR)
  newdat3[i] = mean(sub$TMAX)
  newdat4[i] = mean(sub$TMIN)
}

##Rebinding the newly created vectors back onto the tempsummary data frame
tempsummary$AvgTemp = newdat1
tempsummary$AvgDTR = newdat2
tempsummary$TMAX = newdat3
tempsummary$TMIN = newdat4


write.csv(tempsummary, file = "tempsummary6.csv", append = FALSE, quote = TRUE, sep = " ",
            eol = "\n", na = "NA", dec = ".", row.names = TRUE,
            col.names = TRUE, qmethod = c("escape", "double"),
            fileEncoding = "")

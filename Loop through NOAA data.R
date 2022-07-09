##New Code for NOAA Data###
library(data.table)

noaa = read.csv(file = "/Users/EileenJG/Documents/R work files/NOAA_complete.csv")
noaa$TMAX = noaa$TMAX/10
noaa$TMIN = noaa$TMIN/10
noaa$DTR = noaa$DTR/10


### ENTER DATE RANGE HERE IN APPROPRIATE FORMAT!!!!#######
daterange = c(20130815, 20130828)


unique(noaa$STATION_NAME)

#ENTER STATION NAME AS IT APPEARS IN LIST ABOVE######
sub1 = subset(noaa, noaa$STATION_NAME == "TUCSON INTERNATIONAL AIRPORT AZ US")
sub = subset(sub1, sub1$DATE %between% daterange)

mean(sub$DTR)



#####


daterange = c(20130702, 20130708)


unique(noaa$STATION_NAME)
sub1 = subset(noaa, noaa$STATION_NAME == "NOGALES INTERNATIONAL AIRPORT AZ US")
sub = subset(sub1, sub1$DATE %between% daterange)

mean(sub$DTR)


#####################

noaa = read.csv(file = "/Users/EileenJG/Documents/R work files/tuc2012TempAge.csv")

daterange = c(20130705, 20130718)
unique(noaa$Station)
sub1 = subset(noaa, noaa$Station == "GENERAL IGNACIO P GARCIA INTERNATIONAL MX")
sub = subset(sub1, sub1$DATE %between% daterange)

mean(sub$DTR)

attach (noaa)

abline(lm(Avg.Devel..Temp. , Age.Group),
lines(lowess(Avg.Devel..Temp. , Age.Group)
labels=row.names(Avg.Devel..Temp.))

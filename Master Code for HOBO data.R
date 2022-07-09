####downloaded packages data.table, chron, and car######



###Reading in Master Data#####
library(data.table)
hobo = read.csv(file = "2013_tucson.csv", col.names = c("DateTime", "Temp", "RH", "Site", "City", "Year"))
hobo = hobo[,-5]
hobo$Date = sapply(strsplit(as.character(hobo$DateTime), " "), "[",1)
hobo$Time = sapply(strsplit(as.character(hobo$DateTime), " "), "[",2)
hobo = hobo[,-1]



####Change Values below for Site and Date to work with other parts of the dataset. Everything else should work#####
sub = subset(hobo, hobo$Site == 2 & hobo$Date %between% c("8/16/13", "8/23/13"))

maximum = vector()
minimum = vector()

for(i in 1:length(unique(sub$Date))) {
j = unique(sub$Date)
maximum[i] = max(subset(sub$Temp, sub$Date == j[i]))
minimum[i] = min(subset(sub$Temp, sub$Date == j[i]))
}

spread = maximum - minimum
avgMax = mean(maximum)
avgMin = mean(minimum)



#######Double Loop Functional#####
###Ok, so for the code below, it loops through the data and generates two dataframes called dfM and dfm that have the max and the min for each site (across the top columns) and for each day in the date range specified below. All you have to do if you want to change your data to a different date range (age cohort) is to modify the parameter named "daterange" below, then select it and all the code below it and run it. You can call dfM, dfm and spreads to access the data. -Inf values are because for some dates that don't have temp data
### If you want to run different files ( different years and different city/sites), then you'll need to load them in up at the top of this page by modifying the read.csv command.

##Modify the parameter below to change your age cohorts###########
daterange = c("8/16/13", "8/23/13")


###Setting up dummy lists and some constants.
dataM = list()
datam = list()
k = unique(hobo$Site)

#The double for-loop## of dooooooom!

for(l in 1:length(unique(hobo$Site))) {
sub1 = subset(hobo, hobo$Site == k[l])
sub = subset(sub1, sub1$Date %between% daterange)
	for(i in 1:length(unique(sub$Date))) {
	j = unique(sub$Date)
	maximum[i] = max(subset(sub$Temp, sub$Date == j[i]))
	minimum[i] = min(subset(sub$Temp, sub$Date == j[i]))
}
dataM[[l]] = maximum[-9] 
datam[[l]] = minimum[-9]

}

#Cleaning shit up transposing the dataframe, adding site names as column headers and creating the spread dataframe
dfM = do.call("rbind", dataM)
dfM = t(dfM)
colnames(dfM) = k

dfm = do.call("rbind", datam)
dfm = t(dfm)
colnames(dfm) = k

spreads = dfM-dfm


####attempt1#####

> library(data.table)
> tuc2013 = read.csv(file = "Complete_HOBO_data_T_2013", col.names = c("DateTime", "Temp", "RH", "Site", "City"))

library(data.table)
tuc2013 = read.csv(file = "Complete_HOBO_data_T_2013.csv", col.names = c("DateTime", "Temp", "RH", "Site", "City"))

head(test) = read.csv(file = "Complete_HOBO_data_T_2013.csv", col.names = c("DateTime", "Temp", "RH", "Site", "City"))
head(tuc2013) = read.csv(file = "Complete_HOBO_data_T_2013.csv", col.names = c("DateTime", "Temp", "RH", "Site", "City"))

head(tuc2013)

tuc2013 = tuc2013[,-5]
tuc2013$Date = sapply(strsplit(as.character(tuc2013$DateTime), " "), "[",1)
tuc2013$Time = sapply(strsplit(as.character(tuc2013$DateTime), " "), "[",2)
tuc2013 = tuc2013[,-1]







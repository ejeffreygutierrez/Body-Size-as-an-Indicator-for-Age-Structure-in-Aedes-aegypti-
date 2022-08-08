data <- read.csv(file="predicting age field data.csv")

###for non-normally distributed data: (piecewiseSEM)
library(ggplot2)
library(car)

#for pathway analysis #

library(car)
library(QuantPsyc)
library(ggm)
library(semPlot)
library(lavaan)
library(nlme)
library(devtools)

head(data)
summary(data)
pairs(data) #linear relationships, normal distribution, outliers
myvars <- data[, c(5,7,16)]
pairs(myvars)

#lm#

aLM<-lm(Dev.Avg.Temp.HOBO ~ Length, data=data)
Anova(aLM)
summary(aLM) #gives you coefficients#
coef(aLM)[2]*sd(data$Length)/sd(data$Crowding) #this coefficient defines the relationship between age and cover#

##need a covariance/variance table in the supplementary materials##

#multiple regression#

#lm2#

aLM2<-lm(Length ~ Dev.Avg.Temp.HOBO + Prev1wk.Avg.RH.HOBO + Prev1wk.Avg.Temp.HOBO, data= workshop)
Anova(aLM2)
#standardized coefficients#
cor(data$Dev.Avg.Temp.HOBO, data$Length)
summary(aLM2)$r.squared
lm.beta(aLM2)

#finish the sem#
aLM3<-lm(Avg...Eggs..Female ~ Crowding, data=data)
Anova(aLM3)
summary(aLM3)$r.squared
lm.beta(aLM3)

#exersize#
myvars<-workshop[,c(9,11,16, 22)]
pairs(myvars)

#fit the pieces
lengthLM <- lm(Length ~ Prev1wk.Avg.RH.HOBO, data=data)
length2LM <- lm(Age_SCP ~ Prev1wk.Avg.Temp.HOBO, data =data)
cases <- lm(Age_SCP ~ X._aegypti_fems + Prev1wk.Avg.RH.HOBO + Length, data =data)

head(data)

#evaluate the pieces
Anova(lengthLM)
Anova(length2LM)
Anova(cases)

#standardized coefficients and r2#
lm.beta(lengthLM)
lm.beta(length2LM)
lm.beta(cases)


summary(lengthLM)$r.squared
summary(length2LM)$r.squared
summary(cases)$r.squared

##Determining AIC

library(AICcmodavg)

model1 <- lm(Age_SCP ~ Prev1wk.Avg.RH.HOBO + Dev.Avg.Temp.HOBO, data = data)
model2 <- lm(Age_SCP ~ X._aegypti_fems + Prev1wk.Avg.RH.HOBO + Dev.Avg.Temp.HOBO, data = data)

#define list of models
models <- list(model1, model2)

#specify model names
mod.names <- c('without abundance', 'w abundance')

#calculate AIC of each model
aictab(cand.set = models, modnames = mod.names)




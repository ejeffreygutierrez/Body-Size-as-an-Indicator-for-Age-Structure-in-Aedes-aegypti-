### Load required packages
library(rms)
library(Hmisc)

### Read transcription data from Joy et al. paper
transcription_data <- read.csv("/Users/casch/Documents/Projects/UA\ Dengue/Aedes\ Age\ Structure/Data/From\ Teresa\ 07-07-2017/head\ and\ thorax\ tests\ for\ model.csv", header=TRUE)

### Fit GLM to Joy et al. transcription data to derive prediction model
dd <- datadist(transcription_data)
options(datadist='dd')

fit <- ols(log(age) ~ rcs(gene.expression, 3), data=transcription_data, x=TRUE, y=TRUE, se.fit=TRUE)
predicted <- Predict(fit, gene.expression=seq(min(transcription_data$gene.expression), max(transcription_data$gene.expression), by=0.001))
ggplot(transcription_data) + geom_point(aes(x=gene.expression, y=age)) + geom_line(data=predicted, aes(x=gene.expression, y=exp(yhat)), color=2) + geom_ribbon(data=predicted, aes(x=gene.expression, ymin=exp(lower), ymax=exp(upper)), fill=2, alpha=0.2)

predicted <- Predict(fit, gene.expression=seq(0.052, 8.75, by=0.001))
colnames(predicted) <- c("gene.expression", "age_days_mean", "age_days_lower_95", "age_days_upper_95")
predicted[, 2:4] <- exp(predicted[, 2:4])
predicted <- round(predicted, 4)

write.csv(predicted, "transcription_age.csv")

setwd("D:/Workspace/Personal/RUB2022/VAMoS/StatistikGrundlagen2/Week3")
library(readxl)
library(ggplot2)

ELP <- read.csv("ELP_full_length_frequency.csv", sep = ",", dec= ".", header = TRUE)
SV <- read.csv("winter_2016_senses_valence.csv", sep = ",", dec= ".", header = TRUE)

SV_ELP <- merge(SV, ELP, by = "Word")
SV_ELP <- SV_ELP[SV_ELP$Modality == "Taste"|SV_ELP$Modality == "Smell", ]

lm1 <- lm(RT ~ Log10Freq, data = SV_ELP)
summary(lm1)
cf <- coef(lm1)

newdata = data.frame(Log10Freq=1)
predict(lm1, newdata)

SV_ELP[SV_ELP$Word == "bland",]
lm1$residuals

plot(SV_ELP$Log10Freq, fitted(lm1))
ggplot(SV_ELP, aes(x = Log10Freq, y = RT))+
  geom_point()+
  geom_abline(intercept = 837.27, slope = -74.59, colour = "turquoise3")
resid <- lm1$residuals
sum(lm1$residuals^2)
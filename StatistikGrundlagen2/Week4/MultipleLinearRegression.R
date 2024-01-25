setwd("D:/Workspace/Personal/RUB2022/VAMoS/StatistikGrundlagen2/Week3")
library(readxl)
library(ggplot2)

ELP <- read.csv("ELP_full_length_frequency.csv", sep = ",", dec= ".", header = TRUE)
SV <- read.csv("winter_2016_senses_valence.csv", sep = ",", dec= ".", header = TRUE)

SV_ELP <- merge(SV, ELP, by = "Word")
SV_ELP <- SV_ELP[SV_ELP$Modality == "Taste"|SV_ELP$Modality == "Smell", ]

summary(lm1 <- lm(RT ~ Log10Freq, data = SV_ELP))
summary(lm2 <- lm(RT ~ Log10Freq + length, data = SV_ELP))
summary(lm2 <- lm(scale(RT) ~ scale(Log10Freq) + scale(length), data = SV_ELP))
summary(lm2.1 <- lm(RT ~ Log10Freq * length, data = SV_ELP))
summary(lm3 <- lm(RT ~ Modality, data = SV_ELP))

SV_ELP[SV_ELP$length <= 4, "length_group"] <- "kurz"
SV_ELP[SV_ELP$length == 5, "length_group"] <- "mittel"
SV_ELP[SV_ELP$length >= 6, "length_group"] <- "lang"

SV_ELP$length_group <- as.factor(SV_ELP$length_group)
summary(lm4 <- lm(RT ~ length_group, data = SV_ELP))

SV_ELP$length_group <- factor(SV_ELP$length_group, levels = c("lang", "mittel", "kurz"))

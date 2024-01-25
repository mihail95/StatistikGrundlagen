setwd("D:/Workspace/Personal/RUB2022/VAMoS/StatistikGrundlagen2/Week2")
library(afex)
library(tidyverse)

aoa <- read.csv("aoa_data.csv", dec = ",")
aoa$AOA_SET <- ifelse(aoa$aoa_set == 1, "frueh", "spaet")
aoa$HOMHET <- ifelse(aoa$homhet == 1, "hom", "het")

table(aoa$AOA_SET, aoa$aoa_set)
table(aoa$HOMHET, aoa$homhet)

summary(aov_ez(id = "sub", dv = "rt", data = aoa[aoa$rep != 1, ], within = c("AOA_SET", "HOMHET")))


aoa_agg <- aoa[aoa$rep != 1, ] %>% group_by(sub, AOA_SET, HOMHET) %>% summarize(rt = mean(rt))
aoa_agg <- data.frame(aoa_agg)

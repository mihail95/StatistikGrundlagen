setwd("D:/Workspace/Personal/RUB2022/VAMoS/StatistikGrundlagen2/Week1")
aoa <- read.csv("aoa_data.csv", dec = ",")

aoa_agg <- aggregate(aoa, by=list(aoa$sub, aoa$homhet), FUN = mean)


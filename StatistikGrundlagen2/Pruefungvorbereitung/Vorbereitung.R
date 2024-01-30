data <- c(0.19,0.76,0.85,0.48,0.17,0.37,0.13,0.12,0.37,0.06,0.06,0.21,0.35,0.78,0.73,0.52,0.89,0.78,0.72,0.06,0.03,0.62,0.11,0.93,0.40,0.02,0.70,0.66,0.74,0.53)

length(data)
max(data)
round(mean(data), 2)
round(median(data), 2)
quantile(data)
sd(data)

Warriner <- read_excel("BRM-emot-submit.xlsx")
n <- length(unique(Warriner$Word))

# Korrelationstest - Effektstärke durch r(cor): <0.3; <=0.5, <=1
cor.test(Warriner$V.Mean.Sum, Warriner$A.Mean.Sum)

# T-Tests
t.test(Warriner$V.Mean.F, Warriner$V.Mean.M, alternative = "two.sided", paired = FALSE)
t.test(Warriner$A.Mean.F, Warriner$A.Mean.M, alternative = "two.sided", paired = FALSE)
t.test(Warriner$D.Mean.F, Warriner$D.Mean.M, alternative = "two.sided", paired = FALSE)


# Cohens d: (meanA - meanB)/ sqrt((sd1^2 + sd2^2)/2)
(5.13 - 5.00) / sqrt((1.60^2+1.64^2)/2)
(4.38 - 4.10) / sqrt((2.27^2+2.28^2)/2)
(4.83 - 4.81) / sqrt((2.15^2+2.13^2)/2)


# ANOVA
rd <- read.csv("Beispieldatensatz_A3.csv", sep = ";")
colnames(rd)
rd$negativ + rd$neutral + rd$positiv == rd$ges

# 1 Faktor
aov_ez(id = "vpnr", dv = "ges", data = rd, between = c("bed"))
summary(aov_ez(id = "vpnr", dv = "ges", data = rd, between = c("bed")))

qf(1-0.05, 2, 147)

# 2 Faktor
summary(aov_ez(id = "vpnr", dv = "ges", data = rd, between = c("bed","sex")))

rd_long <- gather(rd, Qualitaet, AnzAdj, 'negativ':'positiv', factor_key=TRUE)

summary(aov_ez(id = "vpnr", dv = "AnzAdj", data = rd_long, within = c("Qualitaet")))
summary(aov_ez(id = "vpnr", dv = "AnzAdj", data = rd_long, between = c("bed"), within = c("Qualitaet")))
setwd("D:/Workspace/Personal/RUB2022/VAMoS/StatistikGrundlagen1/Lecture10")
Ettlinger <- as.data.frame(read_excel("Ettlinger_etal.xlsx"))

## AGL / Exam
#--------------------
# Kreuzproduktsumme
KPSumme <- sum((Ettlinger$AGL - mean(Ettlinger$AGL)) * (Ettlinger$Exam - mean(Ettlinger$Exam)))

# Kovarianz
n <- nrow(Ettlinger)
COVxy <- KPSumme / (n-1)

# Korrelationskoeffizient
r <- COVxy / (sd(Ettlinger$AGL) * sd(Ettlinger$Exam))

# t-Test für Korrelationen
tDF <- (r * sqrt(n-2))/sqrt(1-(r^2))

# kritischer Kennwert
tKrit <- qt(1-0.05, n-2)

# P Wert
pWert <- pt(tDF, n-2, lower.tail = FALSE)

## AGL / Homework
#--------------------
# Kreuzproduktsumme
KPSumme <- sum((Ettlinger$AGL - mean(Ettlinger$AGL)) * (Ettlinger$Homework - mean(Ettlinger$Homework)))

# Kovarianz
n <- nrow(Ettlinger)
COVxy <- KPSumme / (n-1)

# Korrelationskoeffizient
r <- COVxy / (sd(Ettlinger$AGL) * sd(Ettlinger$Homework))

# t-Test für Korrelationen
tDF <- (r * sqrt(n-2))/sqrt(1-(r^2))

# kritischer Kennwert
tKrit <- qt(1-0.05, n-2)

# P Wert
pWert <- pt(tDF, n-2, lower.tail = FALSE)

## IQ/AGL
## Partialkorrelation
## ------------------

# x = AGL ; y = Quiz ; z = IQ
rXY <- (sum((Ettlinger$Quiz - mean(Ettlinger$Quiz)) * (Ettlinger$AGL - mean(Ettlinger$AGL))) / (length(Ettlinger$Quiz) - 1)) / (sd(Ettlinger$Quiz) * sd(Ettlinger$AGL))
rXZ <- (sum((Ettlinger$IQ - mean(Ettlinger$IQ)) * (Ettlinger$AGL - mean(Ettlinger$AGL))) / (length(Ettlinger$IQ) - 1)) / (sd(Ettlinger$IQ) * sd(Ettlinger$AGL))
rYZ <- (sum((Ettlinger$IQ - mean(Ettlinger$IQ)) * (Ettlinger$Quiz - mean(Ettlinger$Quiz))) / (length(Ettlinger$IQ) - 1)) / (sd(Ettlinger$IQ) * sd(Ettlinger$Quiz))

rXYZ <- (rXY - (rYZ* rXZ)) / sqrt((1-rYZ^2)*(1-rXZ^2))
tTestrXYZ <- (rXYZ * sqrt(n-2))/sqrt(1-(rXYZ^2))


## Punktbiseriale Korrelation
## X0 = monolingual und X1 = bilingual

y0 <- Ettlinger[Ettlinger$Biling == "0", ]
y0mean <- mean(y0$AGL)
n0 <- nrow(y0)

y1 <- Ettlinger[Ettlinger$Biling == "1", ]
y1mean <- mean(y1$AGL)
n1 <- nrow(y1)

sdY <- sd(Ettlinger$AGL)


Rpbis <- (y1mean - y0mean)/sdY * (sqrt(n0*n1))/n 

# t-Test für Korrelationen
tDF <- (Rpbis * sqrt(n-2))/sqrt(1-(Rpbis^2))

# kritischer Kennwert
tKrit <- qt(1-0.05, n-2)

# P Wert
pWert <- pt(tDF, n-2, lower.tail = FALSE)

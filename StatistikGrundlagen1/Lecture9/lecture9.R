setwd("D:/Workspace/Personal/RUB2022/VAMoS/StatistikGrundlagen1/Lecture9")
Ettlinger <- as.data.frame(read_excel("Ettlinger_etal.xlsx"))

## Test 1 - Zweistichproben t-Test 
# Build means for mono/bilingual 
x1 <- mean(Ettlinger[ Ettlinger$Biling == 0, "AGL"])
x2 <- mean(Ettlinger[ Ettlinger$Biling == 1, "AGL"])

# Build standard errors
se1 <- var(Ettlinger[Ettlinger$Biling == 0, "AGL"]) / length(Ettlinger[Ettlinger$Biling == 0, "AGL"])
se2 <- var(Ettlinger[Ettlinger$Biling == 1, "AGL"]) / length(Ettlinger[Ettlinger$Biling == 1, "AGL"])

# T-Wert
t_Wert <- (x1 - x2) / sqrt(se1 + se2)

# Degrees of freedom
df <- length(Ettlinger[Ettlinger$Biling == 0, "AGL"]) + length(Ettlinger[Ettlinger$Biling == 1, "AGL"]) - 2

# Kritische T-Wert
t_Krit <- qt(1-0.05/2, df)


## Test 2 - Paardifferenztest
# Mittelwert der Differenzen
d <- mean(Ettlinger$Simple - Ettlinger$Complex)

# Degrees of freedom
df_2 <- 43
n_2 <- 44

# T Wert
t_Wert_2 <- d / sqrt(sum(((Ettlinger$Simple - Ettlinger$Complex) - d)^2)/df_2/n_2)


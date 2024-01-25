setwd("D:/Workspace/Personal/RUB2022/VAMoS/StatistikGrundlagen1/Lecture11")

library(readxl)
library(ppcor)

Passivdaten <- read_excel("Datensatz PassivstudieLH for Limeldas.xlsx")
View(Passivdaten)

Passivdaten[Passivdaten$buch == 99, "buch"] <- NA
Passivdaten[Passivdaten$alter == 99, "alter"] <- NA

Passivdaten <- Passivdaten[complete.cases(Passivdaten), ]
addmargins(table(Passivdaten$klassenstufe, Passivdaten$lhome))
addmargins(table(Passivdaten$buch, Passivdaten$lhome))

(r3.test <- cor.test(Passivdaten$buch, Passivdaten$lhome))
t.test(Passivdaten$TotalScoreTransitivePassivesPercentage[Passivdaten$klassenstufe == 6], 
       Passivdaten$TotalScoreTransitivePassivesPercentage[Passivdaten$klassenstufe == 9], 
       paired = FALSE, alternative = "less")

hist(Passivdaten$TotalScoreTransitivePassivesPercentage[Passivdaten$klassenstufe == 6])
hist(Passivdaten$TotalScoreTransitivePassivesPercentage[Passivdaten$klassenstufe == 9])


t.test(Passivdaten$TotalScoreActivesPercentage[Passivdaten$klassenstufe == 6], 
       Passivdaten$TotalScoreActivesPercentage[Passivdaten$klassenstufe == 9], 
       paired = FALSE, alternative = "two.sided")

hist(Passivdaten$TotalScoreActivesPercentage[Passivdaten$klassenstufe == 6])
hist(Passivdaten$TotalScoreActivesPercentage[Passivdaten$klassenstufe == 9])

# X- Sprache zu Hause; Y- Passivkompetenz; Z-Buchbesitz (method = "spearman")
rXY <- (r3.test <- cor.test(Passivdaten$lhome, Passivdaten$TotalScoreTransitivePassivesPercentage))
rYZ <- (r3.test <- cor.test(Passivdaten$TotalScoreTransitivePassivesPercentage, Passivdaten$buch, method = "spearman", exact=FALSE))
rXZ <- (r3.test <- cor.test(Passivdaten$lhome, Passivdaten$buch, method = "spearman", exact=FALSE))

rXYZ <- (rXY$estimate - (rYZ$estimate*rXZ$estimate))/sqrt((1-rYZ$estimate^2)*(1-rXZ$estimate^2))
t.test(Passivdaten$lhome, 
       Passivdaten$TotalScoreTransitivePassivesPercentage, 
       paired = FALSE, alternative = "two.sided")

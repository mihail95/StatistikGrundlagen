dataset <- read.csv("PastTenseDataBlything.csv", header=TRUE)

summary(glm1 <- glm(RegvsIrreg ~ Age.Years, family=binomial(link = "logit"), data = dataset))

1 / (1 + exp(1)^(-(2.18475031 + 0.07207505 * 4)))

# 0.9205077 vs 0.9222308
# Q1: Warum kommen für x=4 verschiedene Ergebnisse nach unseren Modell glm1 (0.9205077) und die Sigmoid Formel (0.9222308) aus? 
# Slope und Intercept habe ich mit coef(glm1)[1] und coef(glm1)[2] bestimmt (also es muss hier keine Rundung geben).

summary(glm2 <- glm(RegvsIrreg ~ Age.Years + Anal.Reg.AH + Anal.Irreg.AH, family=binomial(link = "logit"), data = dataset))
nung <- dataset[dataset$Stem == "nung", ]
1 / (1 + exp(1)^(-(-0.30067 + 0.07007 * 4 + 5.50663 * 0.5587418 + 2.20595 * 0.02344881)))

shilk <- dataset[dataset$Stem == "shilk", ]
1 / (1 + exp(1)^(-(-0.30067 + 0.07007 * 4 + 5.50663 * 0.3148315 + 2.20595 * 0.03378782)))

trisk <- dataset[dataset$Stem == "trisk", ]
1 / (1 + exp(1)^(-(-0.30067 + 0.07007 * 4 + 5.50663 * 0.5851397 + 2.20595 * 0.01064637)))

anova(glm1, glm2, test="LRT")

summary(glm3 <- glm(RegvsIrreg ~ (Anal.Reg.AH + Anal.Irreg.AH) * Age.Years, family=binomial(link = "logit"), data = dataset))
anova(glm2, glm3, test="LRT")
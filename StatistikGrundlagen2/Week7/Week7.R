#install.packages("lme4")
#install.packages("lmerTest")
#install.packages("lme4", type = "source", lib="C:/Program Files/R/R-4.2.3/library")
setwd("D:/Workspace/Personal/RUB2022/VAMoS/StatistikGrundlagen2/Week7")
library(lme4)
library(dplyr)
library(lmerTest)
library(ggplot2)

Rabovsky_etal <- read.csv("Rabovsky_etal.csv", dec = ",")
Rabovsky_etal$Accuracy01 <- ifelse(Rabovsky_etal$Taste <= 2, 0, 1)


tbl <- table(Rabovsky_etal$VP, Rabovsky_etal$Accuracy01)
prop.table(tbl, 1)

RT_correct <- Rabovsky_etal[Rabovsky_etal$Accuracy01==1, ]
hist(RT_correct$RT, breaks = 40)

RT_correct <- RT_correct[RT_correct$RT >= 300, ]
RT_correct$RT_transformed <- -1/(RT_correct$RT/1000)

  Model_RT1 <- lmer(
    RT_transformed ~ Num_Feats_Tax + sqrt(Density_No_Tax) + Familiarity + ColtheartN + scale(BNC) + 
    (1 + Num_Feats_Tax + sqrt(Density_No_Tax) | VP) + 
    (1 | Objekt), 
    control = lmerControl(optimizer ="bobyqa"), 
    data = RT_correct)

summary(Model_RT1)

Rabovsky_etal <- Rabovsky_etal[Rabovsky_etal$RT >= 300, ]

Model_Accuracy1 <- glmer(
  Accuracy01 ~ Num_Feats_Tax + sqrt(Density_No_Tax) + Familiarity + ColtheartN + scale(BNC) + 
    (1 + sqrt(Density_No_Tax) | VP) + 
    (1 | Objekt), 
  family = binomial,
  control = glmerControl(optimizer ="bobyqa"), 
  data = Rabovsky_etal)

summary(Model_Accuracy1)

# Rabovsky_etal in deciles
Rabovsky_etal$NOFdec <- ntile(Rabovsky_etal$Num_Feats_Tax, 10)
Rabovsky_etal$RTdec <- ntile(Rabovsky_etal$RT, 10)

Rabovsky_dec <- data.frame(
  NOF = aggregate(Rabovsky_etal$Num_Feats_Tax,list(Rabovsky_etal$NOFdec), FUN=mean)$x, 
  RT = aggregate(Rabovsky_etal$RT, list(Rabovsky_etal$NOFdec), FUN=mean)$x
)
#aggregate(Rabovsky_etal$Num_Feats_Tax, list(Rabovsky_etal$NOFdec), FUN=mean)$x
#aggregate(Rabovsky_etal$RT, list(Rabovsky_etal$NOFdec), FUN=mean)$x


ggplot(
  data=Rabovsky_dec, 
  aes(x = NOF, y = RT)) + 
  geom_point() +
  geom_smooth(method = 'glm', level = 0.95, col = 'black')
  

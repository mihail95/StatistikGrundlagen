library(ordinal)
library(lme4)

MI.AI <- read.csv("Renans_etal.csv", fileEncoding = "UTF-8")

MI.AI[MI.AI$Condition == "MI" & MI.AI$Polarity == "Positive" & MI.AI$subject.ID == "CHI3", 1:5]
MI.AI[MI.AI$Condition == "AI" & MI.AI$Polarity == "Positive" & MI.AI$subject.ID == "P11", 1:5]
MI.AI[MI.AI$Condition == "MI" & MI.AI$Polarity == "Negative" & MI.AI$subject.ID == "AD13", 1:5]
MI.AI[MI.AI$Condition == "AI" & MI.AI$Polarity == "Negative" & MI.AI$subject.ID == "P22", 1:5]


MI.AI$Condition <- as.factor(MI.AI$Condition)
MI.AI$Group <- as.factor(MI.AI$Group)
MI.AI$Response <- as.factor(MI.AI$Response)
MI.AI$Polarity <- factor(MI.AI$Polarity, levels = c("Positive", "Negative"))

MI <- subset(MI.AI, MI.AI$Condition=="MI")
AI <- subset(MI.AI, MI.AI$Condition=="AI")

summary(MI.model <- clmm(Response ~ Group*Polarity + (1 + Polarity | subject.ID), data = MI))
summary(AI.model <- clmm(Response ~ Group*Polarity + (1 + Polarity | subject.ID), data = AI))
summary(MI.AI.model <- clmm(Response ~ Group*Polarity*Condition + (1 + Polarity | subject.ID), data = MI.AI))


MI.AI$Inference <- ifelse((MI.AI$Polarity == "Positive" & MI.AI$Response != "3 strawberries") | 
                          (MI.AI$Polarity == "Negative" & MI.AI$Response == "3 strawberries"), 1, 0)

table(MI.AI$Response, MI.AI$Inference, MI.AI$Polarity)

summary(MI.AI.logReg <- glmer(Inference ~ Group*Polarity + Condition + (1 + Polarity | subject.ID),
                              control = glmerControl(optimizer="bobyqa"),
                              family = binomial,
                              data = MI.AI))

Ryan <- read.csv("PastTenseDataBlything.csv", header=TRUE)

Ryan$Year <- 3.5
Ryan$Year[Ryan$AgeGroup == "5_6"] <- 5.5
Ryan$Year[Ryan$AgeGroup == "6_7"] <- 6.5
Ryan$Year[Ryan$AgeGroup == "9_10"] <- 9.5
Ryan$cYear <- Ryan$Year - mean(Ryan$Year)

Ryan_DP <- as.data.frame(xtabs(~Ryan[!is.na(Ryan$RegvsIrreg)== TRUE,"PPT"]))
colnames(Ryan_DP)[1] <- "PPT"
Ryan <- merge(Ryan, Ryan_DP)

library(lme4)
summary(glmer3 <- glmer(RegvsIrreg ~ (Anal.Reg.AH + Anal.Irreg.AH) * cYear 
                        + (1 + Anal.Reg.AH|PPT)
                        + (1 | Stem),
                        family=binomial(link = "logit"),
                        control=glmerControl(optimizer="bobyqa"),
                        data = Ryan))

summary(glmer4 <- glmer(RegvsIrreg ~ (MR.Reg.AH + MR.Irreg.AH) * cYear 
                        + (1 + MR.Reg.AH|PPT)
                        + (1 | Stem),
                        family=binomial(link = "logit"),
                        control=glmerControl(optimizer="bobyqa"),
                        data = Ryan))
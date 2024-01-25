setwd("D:/Workspace/Personal/RUB2022/VAMoS/StatistikGrundlagen1/Lecture6")
Rasch <- read.table("Raschetal.txt", header = TRUE)

Rasch[Rasch$bed == 1, "bed"] <- "strukturell"
Rasch[Rasch$bed == 2, "bed"] <- "bildhaft"
Rasch[Rasch$bed == 3, "bed"] <- "emotional"


tWertBS <- (mean(Rasch[Rasch$bed == "bildhaft", "ges"]) - mean(Rasch[Rasch$bed == "strukturell", "ges"])) / 
  sqrt(var(Rasch[Rasch$bed == "bildhaft", "ges"]) / length(Rasch[Rasch$bed == "bildhaft", "ges"]) +
         var(Rasch[Rasch$bed == "strukturell", "ges"]) / length(Rasch[Rasch$bed == "strukturell", "ges"]))

tWertES <- (mean(Rasch[Rasch$bed == "emotional", "ges"]) - mean(Rasch[Rasch$bed == "strukturell", "ges"])) / 
  sqrt(var(Rasch[Rasch$bed == "emotional", "ges"]) / length(Rasch[Rasch$bed == "emotional", "ges"]) +
         var(Rasch[Rasch$bed == "strukturell", "ges"]) / length(Rasch[Rasch$bed == "strukturell", "ges"]))

tWertEB <- (mean(Rasch[Rasch$bed == "emotional", "ges"]) - mean(Rasch[Rasch$bed == "bildhaft", "ges"])) / 
  sqrt(var(Rasch[Rasch$bed == "emotional", "ges"]) / length(Rasch[Rasch$bed == "emotional", "ges"]) +
         var(Rasch[Rasch$bed == "bildhaft", "ges"]) / length(Rasch[Rasch$bed == "bildhaft", "ges"]))

SCount <- nrow(Rasch[Rasch$bed == "strukturell", ])
BCount <- nrow(Rasch[Rasch$bed == "bildhaft", ])
ECount <-nrow(Rasch[Rasch$bed == "emotional", ])

tWertBSKrit <- qt(0.95, (BCount-1)+(SCount-1))
tWertEBKrit <- qt(0.95, (ECount-1)+(BCount-1))
tWertESKrit <- qt(0.95, (ECount-1)+(SCount-1))

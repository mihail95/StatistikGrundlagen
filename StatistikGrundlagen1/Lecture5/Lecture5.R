setwd("D:/Workspace/Personal/RUB2022/VAMoS/StatistikGrundlagen1/Lecture5")

Teilstudie1 <- as.data.frame(read_excel("Daten Teilstudie 1.xlsx"))
colnames(Teilstudie1) <- c("VerbNr", "Korpusfrequenz_ipm", "Korpusfrequenz_log10ipm", "MittlereSubjFrequenz_Median", "MittlereSubjFrequenz_IQA", "Verb_russisch", "Verb_Uebersetzung")

ggplot(Teilstudie1, aes(x= Korpusfrequenz_log10ipm, y= MittlereSubjFrequenz_Median)) +
  geom_point(size = 2, shape = 19) +
  labs(x = "Korpusfrequenz (log10 ipm)", y = "Mittlere Subjektive Frequenz Median") +
  ggtitle("Logarithmierte Korpusfrequenz und Median der subjektiven Frequenz")

zWerteKF <- scale(Teilstudie1$Korpusfrequenz_log10ipm, center = TRUE, scale = TRUE)
zWerteSF <- scale(Teilstudie1$MittlereSubjFrequenz_Median, center = TRUE, scale = TRUE)
zWerteDF <- data.frame(zWerteKF, zWerteSF)

ggplot(zWerteDF, aes(x= zWerteKF, y= zWerteSF)) +
  geom_point(size = 2, shape = 19) +
  labs(x = "Korpusfrequenz (log10 ipm)", y = "Mittlere Subjektive Frequenz Median") +
  ggtitle("Logarithmierte Korpusfrequenz und Median der subjektiven Frequenz")

zWerteDF[which(Teilstudie1$Verb_Uebersetzung == "hinzufügen"), ]
zWerteDF[which(Teilstudie1$Verb_Uebersetzung == "schlummern"), ]
zWerteDF[which(Teilstudie1$Verb_Uebersetzung == "laut lachen"), ]

zWerteDF$diff <- zWerteKF - zWerteSF

round(mean(Teilstudie1[Teilstudie1$Frequenztyp == "homogen" , 4]), digits = 3)
round(mean(Teilstudie1[Teilstudie1$Frequenztyp == "heterogen" , 4]), digits = 3)
round(mean(Teilstudie1[Teilstudie1$Frequenztyp == "homogen" , 5]), digits = 3)
round(mean(Teilstudie1[Teilstudie1$Frequenztyp == "heterogen" , 5]), digits = 3)

Teilstudie3 <- as.data.frame(read_excel("Daten Teilstudie 3.xlsx"))
round(mean(Teilstudie3[Teilstudie3$Frequenztyp == "homogen" , 4]), digits = 3)
round(mean(Teilstudie3[Teilstudie3$Frequenztyp == "heterogen" , 4]), digits = 3)
round(mean(Teilstudie3[Teilstudie3$Frequenztyp == "homogen" , 5]), digits = 3)
round(mean(Teilstudie3[Teilstudie3$Frequenztyp == "heterogen" , 5]), digits = 3)

# Zusatzaufgabe
table1 <- read_excel("Tabelle1.xlsx")
table1[ , "Prozent"] <- round(table1[ , "Anzahl"] / table1[ , "Kategoriengröße"] * 100, digits = 2)

ggplot(table1, aes(x=Konstituententyp, y=Prozent, group = factor(Wortfolge), fill= factor(Wortfolge))) +
  geom_col(position=position_dodge(), width = 0.8) +
  geom_text(aes(label = Prozent), vjust=-0.5, position=position_dodge(width=0.8), size=4.0) +
  labs(y = "Prozent",
       x = "Konstituententyp",
       fill = "Wortfolge")+
  scale_y_continuous(expand = c(0,0), limits = c(0, 100))+
  theme(legend.position = "bottom")+
  scale_fill_manual(values = c("turquoise3", "turquoise4"), labels = c("OV", "VO"))

table1$Konstituententyp <- factor(table1$Konstituententyp, levels = c("Pronomen", "Adverb", "indefinite NP", "Eigenname", "Adjektiv", "PP", "definite NP"))
ggplot(table1, aes(x=Konstituententyp, y=Prozent, group = factor(Wortfolge), fill= factor(Wortfolge))) +
  geom_col(position=position_dodge(), width = 0.8) +
  geom_text(aes(label = Prozent), vjust=-0.5, position=position_dodge(width=0.8), size=4.0) +
  labs(y = "Prozent",
       x = "Konstituententyp",
       fill = "Wortfolge")+
  scale_y_continuous(expand = c(0,0), limits = c(0, 100))+
  theme(legend.position = "bottom")+
  scale_fill_manual(values = c("turquoise3", "turquoise4"), labels = c("OV", "VO"))

(K_Typ <- ggplot(table1, aes(x=Konstituententyp, y=Prozent, group = factor(Wortfolge), fill= factor(Wortfolge))) +
    geom_col(position=position_dodge(), width = 0.8) +
    geom_text(aes(label = Anzahl), vjust=-0.5, position=position_dodge(width=0.8), size=4.0) +
    labs(y = "Prozent",
         x = "Konstituententyp",
         fill = "Wortfolge")+
    scale_y_continuous(limits = c(0, 100))+
    theme(legend.position = "bottom")+
    scale_fill_manual(values = c("turquoise3", "turquoise4"), labels = c("OV", "VO")))


# Zusatzaufgabe 2
table2 <- read_excel("Tabelle2.xlsx")
table2[ , "Prozent"] <- round(table2[ , "Anzahl"] / table2[ , "Kategoriengröße"] * 100, digits = 2)

ggplot(table2, aes(x=Konstituentenlänge, y=Prozent, group = factor(Wortfolge), fill= factor(Wortfolge))) +
  geom_col(position=position_dodge(), width = 0.8) +
  geom_text(aes(label = Prozent), vjust=-0.5, position=position_dodge(width=0.8), size=4.0) +
  labs(y = "Prozent",
       x = "Konstituentenlänge",
       fill = "Wortfolge")+
  scale_y_continuous(limits = c(0, 100))+
  theme(legend.position = "bottom")+
  scale_fill_manual(values = c("turquoise3", "turquoise4"), labels = c("OV", "VO"))

(K_Länge <- ggplot(table2, aes(x=Konstituentenlänge, y=Prozent, group = factor(Wortfolge), fill= factor(Wortfolge))) +
    geom_col(position=position_dodge(), width = 0.8) +
    geom_text(aes(label = Prozent), vjust=-0.5, position=position_dodge(width=0.8), size=4.0) +
    labs(y = "Prozent",
         x = "Konstituentenlänge",
         fill = "Wortfolge")+
    scale_y_continuous(limits = c(0, 100))+
    theme(legend.position = "bottom")+
    scale_fill_manual(values = c("turquoise3", "turquoise4"), labels = c("OV", "VO")))

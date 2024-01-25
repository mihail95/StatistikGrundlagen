## Diese Pakete werden für die Analyse benötigt; falls weitere Pakete benötigt werden, einfach hinzufügen:

# Pakete laden:
library(ggplot2)
library(lme4)

## Jetzt die Daten einlesen. Der Datensatz heißt Brandt_data.csv und ist im Moodle-Kurs hinterlegt.

## Nach dem Einlesen sollte der Datensatz 5382 Zeilen und 20 Spalten haben.

# Datensatz einlesen:
?read.csv
df <- read.csv("Brandt_data.csv", col.names = c("Subject","AgeGroup","Phase","Item_ID", "prime_target",
                                                "PrimeType","TargetSlide.ACC","Sex","Procedure.Block.","Trial",
                                                "picture1","picture2","PracticeSlide.ACC","PracticeSlide.CRESP","richtig",
                                                "Sortiernummer","TargetSlide.CRESP","testpic1","testpic2","text"))
df1 <- read.csv("Brandt_data.csv")

## In der Sitzung am 14.12.2023 haben wir erarbeitet, dass das Design wie folgt aufgebaut ist:
##   
## Die drei Phasen werden direkt hintereinander bei jedem Kind erhoben. In der Baseline-Phase 
## gibt es nur ambige Durchgänge, sie dienen dazu, zu ermitteln, wie oft sich die Kinder für die 
## Objektrelativsatz-Lesart entscheiden. 
## 
## Die Priming-Phase ist eine Art Trainingsphase: Hier werden die Lesarten in den Prime-Durchgängen 
## a) nur durch die Bilder disambiguiert, b) durch die Bilder und einen eindeutigen Kasus (Akkusativ) 
## im Relativpronomen disambiguiert oder c) durch die Bilder disambiguiert, wobei die NPs im Prime- 
## und Zielsatz identisch sind. Die Bedingung c) liegt uns in unseren Daten nicht vor. 
## 
## Die Post-Test-Phase ist eine Phase, in der ermittelt wird, ob sich die Kinder im Vergleich zur 
## Baseline-Phase durch das Training in ihrer Interpretation von ambigen Relativsätzen verändert haben. 
## 
## Die Autor:innen manipulieren die unabhängigen Variablen wie folgt: 
##   - Phase: 			innerhalb Versuchspersonen, zwischen Items
## - Primetyp / Bedingung: 	zwischen Versuchspersonen, innerhalb Items
## - Alter: 			zwischen Versuchspersonen, innerhalb Items
## Diese Angaben benötigen Sie, um die zufälligen Effekte (random slopes) korrekt zu spezifizieren. 


############################
## Datensatz kennenlernen ##
############################

## Um die Struktur der Daten kennenzulernen, stelle ich hier ein paar fertige Kommandos bereit.
## Dieser Teil ist OPTIONAL und kann vollständig ausgelassen werden; er dient nur als Hilfe, 
## um sich in den Daten zurechtzufinden. Der untere Code geht davon aus, dass der Datensatz
## Brandt_data.csv in der Variable df gespeichert wurde.
##
## Eine wichtige Vorbemerkung ist, dass uns nicht exakt derselbe Datensatz wie den AutorInnen
## der Studie vorliegt. Im Paper wird berichtet, dass drei unterschiedliche Primetypen getestet
## wurden. Die drei Ausprägung dieses Faktors heißen (1) "ambiguous primes condition", 
## (2) "case disambiguated primes condition" und (3) "NP1 overlap condition" (siehe Tabelle 1, S. 249).
## Uns liegen nur Daten für die ersten beiden Ausprägungen vor, was wir anhand der Ausprägungen
## in der Spalte PrimeType überprüfen können:

unique(df$PrimeType)

## Dass uns die NP1 overlap condition fehlt, hat natürlich zur Folge, dass sich die Struktur
## unserer Daten stellenweise von dem, was im Paper berichtet wird, unterscheidet. Zum Beispiel
## heißt es im Abschnitt Participants, dass 105 Kinder am Experiment teilgenommen haben (S. 246).
## Wenn wir überprüfen, wie viele unterschiedliche Einträge es in der Spalte Subject gibt,
## sehen wir allerdings eine andere Zahl:

length(unique(df$Subject))

## Unser Datensatz enthält also nur Daten von 69 Kindern. Diese Zahl passt jedoch wieder,
## wenn wir uns in Tabelle 1 (S. 249) ansehen, wie viele Kinder in den drei Ausprägungen
## getestet wurden: In der Ausprägung (1) "ambiguous primes condition" wurden 15 Kinder
## im Alter von sechs Jahren und 18 Kinder im Alter von neun Jahren getestet; in der
## Ausprägung (2) "case disambiguated primes condition" wurden je 18 Kinder im Alter von
## sechs und neun Jahren getestet. Insgesamt kommt man also auf 69 Kinder, die in diesen
## beiden Ausprägungen getestet wurden. In dieser Hinsicht sind unsere Daten demnach
## vollständig.
##
## Wir könnten bei der Kontrolle der Daten natürlich noch weiter ins Detail gehen,
## zum Beispiel indem wir die Gruppengrößen, die in Tabelle 1 berichtet werden, überprüfen:

length(unique(df[df$PrimeType == 1 & df$AgeGroup == 6, "Subject"]))
length(unique(df[df$PrimeType == 2 & df$AgeGroup == 6, "Subject"]))
length(unique(df[df$PrimeType == 1 & df$AgeGroup == 9, "Subject"]))
length(unique(df[df$PrimeType == 2 & df$AgeGroup == 9, "Subject"]))

## Auch hier stimmen die Zahlen aus dem Paper mit dem Teil der Daten, die uns zur Verfügung stehen,
## überein.
##
## Als nächstes schauen wir uns die Items etwas genauer an. Im Abschnitt Materials (S. 246 f.) wird
## berichtet, dass im Experiment 78 Sätze als Stimuli verwendet wurden. 36 davon waren die Targets,
## 12 waren Primes und die übrigen 30 waren Filler. Ob ein Item zu den Targets, den Primes oder den
## Fillern gehört, wird in der Spalte prime_target festgehalten.

unique(df$prime_target)

## Wir wissen bereits, dass die Ausprägung "1" hier für die Primes steht, "2" für die Targets
## und "10" für die Filler. Trotzdem lohnt es sich auch hier, das noch einmal zu überprüfen,
## zum Beispiel indem wir ansehen, wie viele unterschiedliche Items es in diesen drei
## Ausprägungen gibt:

length(unique(df[df$prime_target ==  1, "Item_ID"]))
length(unique(df[df$prime_target ==  2, "Item_ID"]))
length(unique(df[df$prime_target == 10, "Item_ID"]))

## Wir sehen 24 Items, die in der Spalte prime_target die Ausprägung "1" haben; 36 Items haben
## die Ausprägung "2", und 30 Items haben die Ausprägung "10". Bei den Targets und Fillern
## stimmen diese Zahlen genau zu dem, was das Paper berichtet. Aber wieso sind in unseren Daten
## 24 Primes, wo im Paper doch nur von 12 die Rede ist?
##
## Um das zu verstehen, müssen wir uns noch einmal in Erinnerung rufen, wie viele Ausprägungen
##der Faktor Primetyp hat, und uns dann überlegen, wodurch sich die Ausprägungen dieses Faktors
## eigentlich unterscheiden. Die Antwort lautet natürlich: Durch die verwendeten Primes!
##
## Die drei (in den uns vorliegenden Daten: zwei) Ausprägungen des Faktors Primetyp unterscheiden
## sich dadurch, dass dort verschiedene Primes gezeigt wurden. Das erklärt, weshalb wir in unseren
## Daten 24 verschiedene Items zählen, die in der Spalte prime_target die Ausprägung "1" haben:
## 12 davon gehören zur PrimeType-Ausprägung "1" und die anderen 12 gehören zur
## PrimeType-Ausprägung "2":

length(unique(df[df$PrimeType == 1 & df$prime_target == 1, "Item_ID"]))
length(unique(df[df$PrimeType == 2 & df$prime_target == 1, "Item_ID"]))

## Und so stimmen Datensatz und Paper nun auch wieder überein.

## Im Abschnitt Procedure (S. 249 ff.) wird schließlich berichtet, dass das Experiment in
## drei Phasen durchgeführt wurde. Die Reihenfolge war immer dieselbe, sodass Kinder als
## erstes in der "baseline phase", danach in der "prime phase" und zum Abschluss in der
## "post-test phase" getestet wurden. Im Datensatz werden diese drei Ausprägungen in der
## Spalte Phase festgehalten:

unique(df$Phase)

## Weiterhin heißt es, dass die 36 Targets gleichmäßig auf alle drei Testphasen aufgeteilt wurden.
## Zusätzlich wurden in der zweiten Phase die 12 Primes verwendet. Wie genau die Filler aufgeteilt
## wurden, steht nicht im Paper, kann aber mit der nachfolgenden Tabelle angesehen werden:

table(df[df$Subject == 1, "prime_target"], df[df$Subject == 1, "Phase"])

## Die Tabelle zeigt, dass in Phase 1 und 3 jeweils 8 Filler gezeigt wurden. In der Prime-Phase
## wurden etwas mehr Filler verwendet, was vermutlich daran liegt, dass durch die Primes auch
## insgesamt mehr Items gezeigt wurden. Wie erwartet sieht man außerdem, dass die Primes nur in
## Phase 2 gezeigt wurden, und dass die Targets in der Tat gleichmäßig über die drei Phasen
## verteilt wurden.


#################
## Analyseteil ##
#################

## Hier beginnt der eigentliche Teil eurer Aufgabe. Als erstes solltet ihr ein Subset der Daten
## erstellen, das nur noch die Zeilen mit Targets enthält, Primes und Filler sollten nicht mehr
## enthalten sein.

# Subset erstellen; darin aller Primes und Filler rauswerfen.
targetsDF <- df[df$prime_target == 2, ]

## Als nächstes solltet ihr einige Variablen so umkodieren, dass sie der Kodierung im Paper
## entsprechen (im Abschnitt Results, S. 250). Achtung: Bei der Variable Primetyp weichen wir
## vom Paper ab, da uns nur zwei der drei Ausprägungen vorliegen.

# Age: 6 als -0.5 und 9 als 0.5 kodieren
targetsDF$AgeGroup[targetsDF$AgeGroup == 6] <- -0.5
targetsDF$AgeGroup[targetsDF$AgeGroup == 9] <- 0.5

# Phase: 1 als -0.5, 2 als 0 und 3 als 0.5 kodieren
targetsDF$Phase[targetsDF$Phase == 1] <- -0.5
targetsDF$Phase[targetsDF$Phase == 2] <- 0
targetsDF$Phase[targetsDF$Phase == 3] <- 0.5

# PrimeType: 1 als -0.5 und 2 als 0.5 kodieren
targetsDF$PrimeType[targetsDF$PrimeType == 1] <- -0.5
targetsDF$PrimeType[targetsDF$PrimeType == 2] <- 0.5

###############################
## Gemischte Modlle - Teil 1 ##
###############################

## Als nächstes solltet ihr mit der im Paper genannten "Forward Selection"-Methode
## nach und nach die berichteten Modelle nachbauen. Noch einmal zur Erinnerung:
## "Forward Selection" meint, dass ihr mit dem Nullmodell beginnt, welches nur die
## zufälligen Effekte enthält, und danach einzeln die Haupteffekte und Interaktionen
## hinzufügt. Haltet euch hierbei an das Vorgehen im Abschnitt Results (S. 250).
##
## Noch ein Hinweis zu den zufälligen Effekten: Hier solltet ihr zufällige Intercepts
## sowohl für Versuchspersonen als auch für Items in das Modell aufnehmen. Zufällige
## Slopes solltet ihr für alle Faktoren aufnehmen, die innerhalb getestet wurden. Das
## Alter der Kinder wurde bspw. zwischen den Versuchspersonen getestet, weshalb hierfür
## kein zufällige Slope berechnet werden soll. Aber wie sieht es hier bei den Items aus?
## Und wie wurden die übrigen Variablen getestet? Das müsst ihr klären, bevor ihr das
## Modell aufstellt.

# Random Intercepts: Item_ID; Subject
# Random Random Slopes for: 1+Phase|Subject; 1+PrimeType|Item_ID; 1+AgeGroup|Item_ID
# AV: TargetSlide.ACC (1 = Object RC, 0 = Subject RC)

# Nullmodell berechnen (nur Random Effects / zufällige Effekte)
?glmer
#nullModel <- glmer(TargetSlide.ACC ~ (1|Item_ID) + (1|Subject) +
#                     (1+Phase|Subject) + (1+PrimeType+AgeGroup|Item_ID),
#                   control = glmerControl(optimizer="bobyqa"),
#                   family = binomial,
#                   data = targetsDF)
nullModel <- glmer(TargetSlide.ACC ~ (1+Phase|Subject) + (1+PrimeType+AgeGroup|Item_ID),
                   control = glmerControl(optimizer="bobyqa"),
                   family = binomial,
                   data = targetsDF)

## Wenn ihr das Nullmodell aufgestellt habt, könnt ihr den ersten Haupteffekt,
## der im Paper berichtet wird, hinzufügen. Anschließend solltet ihr einen
## Likelihood-Ratio-Test ausführen, um die beiden Modelle miteinander zu vergleichen.

# Modell 1 (Nullmodell + Haupteffekt 1)
#model1 <- glmer(TargetSlide.ACC ~ Phase + (1|Item_ID) + (1|Subject) +
#                     (1+Phase|Subject) + (1+PrimeType+AgeGroup|Item_ID),
#                   control = glmerControl(optimizer="bobyqa"),
#                   family = binomial,
#                   data = targetsDF)
model1 <- glmer(TargetSlide.ACC ~ Phase +(1+Phase|Subject) + (1+PrimeType+AgeGroup|Item_ID),
                   control = glmerControl(optimizer="bobyqa"),
                   family = binomial,
                   data = targetsDF)

# Likelihood-Ratio-Test (LRT) durchführen
?anova
anova(nullModel, model1, test='Chisq')

## Hiernach wiederholt ihr die Schritte so häufig, bis ihr bei dem Modell angekommen
## seid, für welches die AutorInnen berichten, dass es keine Verbesserung mehr gegeben
## hat. Insgesamt solltet ihr in diesem Abschnitt fünf Modelle bauen (das Nullmodell
## eingeschlossen):

# Modell 2 (Modell 1 + Haupteffekt 2)
#model2 <- glmer(TargetSlide.ACC ~ AgeGroup + Phase + (1|Item_ID) + (1|Subject) +
#                  (1+Phase|Subject) + (1+PrimeType+AgeGroup|Item_ID),
#                control = glmerControl(optimizer="bobyqa"),
#                family = binomial,
#                data = targetsDF)
model2 <- glmer(TargetSlide.ACC ~ AgeGroup + Phase +
                  (1+Phase|Subject) + (1+PrimeType+AgeGroup|Item_ID),
                control = glmerControl(optimizer="bobyqa"),
                family = binomial,
                data = targetsDF)
# LRT
anova(model1,model2, test='Chisq')

# Modell 3 (Modell 2 + Interaktion)
#model3 <- glmer(TargetSlide.ACC ~ Phase*AgeGroup + (1|Item_ID) + (1|Subject) +
#                  (1+Phase|Subject) + (1+PrimeType+AgeGroup|Item_ID),
#                control = glmerControl(optimizer="bobyqa"),
#                family = binomial,
#                data = targetsDF)
model3 <- glmer(TargetSlide.ACC ~ Phase*AgeGroup +  
                  (1+Phase|Subject) + (1+PrimeType+AgeGroup|Item_ID),
                control = glmerControl(optimizer="bobyqa"),
                family = binomial,
                data = targetsDF)
# LRT
anova(model2,model3, test='Chisq')

# Modell 4 (Modell 3 + Haupteffekt 3)
#model4 <- glmer(TargetSlide.ACC ~ Phase*AgeGroup + PrimeType + (1|Item_ID) + (1|Subject) +
#                  (1+Phase|Subject) + (1+PrimeType+AgeGroup|Item_ID),
#                control = glmerControl(optimizer="bobyqa"),
#                family = binomial,
#                data = targetsDF)

model4 <- glmer(TargetSlide.ACC ~ Phase*AgeGroup + PrimeType  +
                  (1+Phase|Subject) + (1+PrimeType+AgeGroup|Item_ID),
                control = glmerControl(optimizer="bobyqa"),
                family = binomial,
                data = targetsDF)
# LRT
anova(model3,model4, test='Chisq')

###############################
## Gemischte Modlle - Teil 2 ##
###############################

## Für die zweiten Analyse haben die AutorInnen Untergruppen gebildet, und zwar je eine
## Gruppe pro Altersstufe-Primetyp-Kombination (S. 250 f.). Im Paper wird hier von sechs
## Gruppen gesprochen, da uns allerdings die "NP1 overlap"-Daten fehlen, können wir
## diese Analyse nur für vier Gruppen replizieren. Für eine Übersicht über die Gruppen,
## könnt ihr euch Tabelle 3 im Paper ansehen (S. 251).
##
## Für jede Untergruppe solltet ihr jeweils zwei Modelle bauen, das Nullmodell und darauf
## aufbauen ein Modell, dass den Faktor Phase enthält. Führt außerdem je einen
## Likelihood-Ratio-Test zwischen den beiden Modellen aus, um sie zu miteinandner vergleichen.
##
## Ein Hinweis zu den zufälligen Effekten: Bei den Modellen hier geben wir vor, dass ihr nur
## zufällige Intercepts für Versuchspersonen und Items in das Modell aufnehmen müsst Zufällige
## Slopes könnt ihr weglassen.

# PrimeType: "ambiguous primes condition" = -0.5
#            "case disambiguated primes condition" = 0.5
# Age: 6 = -0.5; 9 = 0.5 

# Gruppe 1: Age 6, ambig
group1 <- targetsDF[targetsDF$AgeGroup == -0.5 & targetsDF$PrimeType == -0.5, ]
# Nullmodell (nur zufällige Intercepts)
nullModelGrp1 <- glmer(TargetSlide.ACC ~ (1|Item_ID) + (1|Subject),
                           control = glmerControl(optimizer="bobyqa"),
                           family = binomial,
                           data = group1)
# Modell 1 (Nullmodell + Phase als fester Effekt)
model1Grp1 <- glmer(TargetSlide.ACC ~ Phase + (1|Item_ID) + (1|Subject),
                       control = glmerControl(optimizer="bobyqa"),
                       family = binomial,
                       data = group1)
# LRT
anova(nullModelGrp1,model1Grp1, test='Chisq')

# Gruppe 2: Age 6, case
group2 <- targetsDF[targetsDF$AgeGroup == -0.5 & targetsDF$PrimeType == 0.5, ]
# Nullmodell (nur zufällige Intercepts)
nullModelGrp2 <- glmer(TargetSlide.ACC ~ (1|Item_ID) + (1|Subject),
                       control = glmerControl(optimizer="bobyqa"),
                       family = binomial,
                       data = group2)
# Modell 1 (Nullmodell + Phase als fester Effekt)
model1Grp2 <- glmer(TargetSlide.ACC ~ Phase + (1|Item_ID) + (1|Subject),
                    control = glmerControl(optimizer="bobyqa"),
                    family = binomial,
                    data = group2)
# LRT
anova(nullModelGrp2,model1Grp2, test='Chisq')

# Gruppe 3: Age 9, ambig
group3 <- targetsDF[targetsDF$AgeGroup == 0.5 & targetsDF$PrimeType == -0.5, ]
# Nullmodell (nur zufällige Intercepts)
nullModelGrp3 <- glmer(TargetSlide.ACC ~ (1|Item_ID) + (1|Subject),
                       control = glmerControl(optimizer="bobyqa"),
                       family = binomial,
                       data = group3)
# Modell 1 (Nullmodell + Phase als fester Effekt)
model1Grp3 <- glmer(TargetSlide.ACC ~ Phase + (1|Item_ID) + (1|Subject),
                    control = glmerControl(optimizer="bobyqa"),
                    family = binomial,
                    data = group3)
# LRT
anova(nullModelGrp3,model1Grp3, test='Chisq')


# Gruppe 4: Age 9, case
group4 <- targetsDF[targetsDF$AgeGroup == 0.5 & targetsDF$PrimeType == 0.5, ]
# Nullmodell (nur zufällige Intercepts)
nullModelGrp4 <- glmer(TargetSlide.ACC ~ (1|Item_ID) + (1|Subject),
                       control = glmerControl(optimizer="bobyqa"),
                       family = binomial,
                       data = group4)
# Modell 1 (Nullmodell + Phase als fester Effekt)
model1Grp4 <- glmer(TargetSlide.ACC ~ Phase + (1|Item_ID) + (1|Subject),
                    control = glmerControl(optimizer="bobyqa"),
                    family = binomial,
                    data = group4)
# LRT
anova(nullModelGrp4,model1Grp4, test='Chisq')


################################
## Gemischte Modelle - Teil 3 ##
################################

## In diesem Teil wurden für die neunjährigen Kinder noch einmal weitere Untergruppen
## gebildet (siehe Tabelle 4, S. 251). Auch hier solltet ihr nach dem oberen Vorgehen
## pro Untergruppe jeweils das Nullmodell mit dem Modell, das Phase als festen Effekt
## enthält, vergleichen.

# Phase: 1 als -0.5, 2 als 0 und 3 als 0.5 kodieren
# "baseline phase", "prime phase", "post-test phase"

# Gruppe 1: ambig, base vs. prime
group5 <- targetsDF[targetsDF$AgeGroup == 0.5 & targetsDF$PrimeType == -0.5 & targetsDF$Phase != 0.5, ]
# Nullmodell (nur zufällige Intercepts)
nullModelGrp5 <- glmer(TargetSlide.ACC ~ (1 + Phase|Item_ID) + (1 + Phase|Subject),
                       control = glmerControl(optimizer="bobyqa"),
                       family = binomial,
                       data = group5)
# Modell 1 (Nullmodell + Phase als fester Effekt)
model1Grp5 <- glmer(TargetSlide.ACC ~ Phase + (1 + Phase|Item_ID) + (1 + Phase|Subject),
                    control = glmerControl(optimizer="bobyqa"),
                    family = binomial,
                    data = group5)
# LRT
anova(nullModelGrp5,model1Grp5, test='Chisq')


# Gruppe 2: ambig, base vs. post
group6 <- targetsDF[targetsDF$AgeGroup == 0.5 & targetsDF$PrimeType == -0.5 & targetsDF$Phase != 0, ]
# Nullmodell (nur zufällige Intercepts)
nullModelGrp6 <- glmer(TargetSlide.ACC ~ (1|Item_ID) + (1|Subject),
                       control = glmerControl(optimizer="bobyqa"),
                       family = binomial,
                       data = group6)
# Modell 1 (Nullmodell + Phase als fester Effekt)
model1Grp6 <- glmer(TargetSlide.ACC ~ Phase + (1|Item_ID) + (1|Subject),
                    control = glmerControl(optimizer="bobyqa"),
                    family = binomial,
                    data = group6)
# LRT
anova(nullModelGrp6,model1Grp6, test='Chisq')


# Gruppe 3: case, base vs. prime
group7 <- targetsDF[targetsDF$AgeGroup == 0.5 & targetsDF$PrimeType == 0.5 & targetsDF$Phase != 0.5, ]
# Nullmodell (nur zufällige Intercepts)
nullModelGrp7 <- glmer(TargetSlide.ACC ~ (1|Item_ID) + (1|Subject),
                       control = glmerControl(optimizer="bobyqa"),
                       family = binomial,
                       data = group7)
# Modell 1 (Nullmodell + Phase als fester Effekt)
model1Grp7 <- glmer(TargetSlide.ACC ~ Phase + (1|Item_ID) + (1|Subject),
                    control = glmerControl(optimizer="bobyqa"),
                    family = binomial,
                    data = group7)
# LRT
anova(nullModelGrp7,model1Grp7, test='Chisq')


# Gruppe 4: case, base vs. post
group8 <- targetsDF[targetsDF$AgeGroup == 0.5 & targetsDF$PrimeType == 0.5 & targetsDF$Phase != 0, ]
# Nullmodell (nur zufällige Intercepts)
nullModelGrp8 <- glmer(TargetSlide.ACC ~ (1|Item_ID) + (1|Subject),
                       control = glmerControl(optimizer="bobyqa"),
                       family = binomial,
                       data = group8)
# Modell 1 (Nullmodell + Phase als fester Effekt)
model1Grp8 <- glmer(TargetSlide.ACC ~ Phase + (1|Item_ID) + (1|Subject),
                    control = glmerControl(optimizer="bobyqa"),
                    family = binomial,
                    data = group8)
# LRT
anova(nullModelGrp8,model1Grp8, test='Chisq')


################################
## Gemischte Modelle - Teil 4 ##
################################

## Zum Abschluss werden noch einmal gemischte Modelle für Untergruppen berechnet und
##verglichen, dieses Mal muss allerdings ein gänzlich neuer Faktor kodiert werden.
## Die AutorInnen berichten, dass sie in den Untergruppen der neunjährigen Kinder
## getestet haben, ob das Priming in der ersten Hälfte einer Testphase einen anderen
## Effekt hat als in der zweiten Hälfte derselben Phase (S. 252). Für diese Modelle muss
## also eine neue kategorielle Variable mit zwei Ausprägungen erstellt werden;
## die Targets 1 - 6 einer Phase werden der ersten Ausprägung zugeordnet, während die
## Targets 7 - 12 einer Phase der zweiten Ausprägung zugeordnet werden.

teil4DF <- targetsDF
# Neue Kategorielle Variable erstellen

teil4DF$trialFactor <- 0

# There's probably a better way to do this, but here we goooo
# For every subject, for every phase, cut the trials into two equal parts and label the 
# first half as 1 and the second half as 2

for (subject in unique(teil4DF$Subject)) {
  for (phase in unique(teil4DF$Phase)) {
    trials <- teil4DF[teil4DF$Subject == subject & teil4DF$Phase == phase, "Trial"]
    teil4DF[teil4DF$Subject == subject & teil4DF$Phase == phase, "trialFactor"] <- cut(trials, breaks = 2, labels = FALSE)
  }
}

## Diese Analyse wurde nur für die neunjährigen und nur in den Phasen 2 und 3 durchgeführt. (Phase != -0.5)
teil4DF <- teil4DF[teil4DF$AgeGroup == 0.5 & teil4DF$Phase != -0.5, ]

# Phase: 1 als -0.5, 2 als 0 und 3 als 0.5 kodieren
# "baseline phase", "prime phase", "post-test phase"

# PrimeType: "ambiguous primes condition" = -0.5
#            "case disambiguated primes condition" = 0.5

# Gruppe 1: ambig, prime
group9 <- teil4DF[teil4DF$PrimeType == -0.5 & teil4DF$Phase == 0, ]
# Nullmodell (nur zufällige Intercepts)
nullModelGrp9 <- glmer(TargetSlide.ACC ~ (1|Item_ID) + (1|Subject),
                       control = glmerControl(optimizer="bobyqa"),
                       family = binomial,
                       data = group9)
# Modell 1 (Nullmodell + neuer Faktor als fester Effekt)
model1Grp9 <- glmer(TargetSlide.ACC ~ trialFactor + (1|Item_ID) + (1|Subject),
                    control = glmerControl(optimizer="bobyqa"),
                    family = binomial,
                    data = group9)
# LRT
anova(nullModelGrp9,model1Grp9, test='Chisq')

# Gruppe 2: ambig, post
group10 <- teil4DF[teil4DF$PrimeType == -0.5 & teil4DF$Phase == 0.5, ]
# Nullmodell (nur zufällige Intercepts)
nullModelGrp10 <- glmer(TargetSlide.ACC ~ (1|Item_ID) + (1|Subject),
                       control = glmerControl(optimizer="bobyqa"),
                       family = binomial,
                       data = group10)
# Modell 1 (Nullmodell + neuer Faktor als fester Effekt)
model1Grp10 <- glmer(TargetSlide.ACC ~ trialFactor + (1|Item_ID) + (1|Subject),
                    control = glmerControl(optimizer="bobyqa"),
                    family = binomial,
                    data = group10)
# LRT
anova(nullModelGrp10,model1Grp10, test='Chisq')

# Gruppe 3: case, prime
group11 <- teil4DF[teil4DF$PrimeType == 0.5 & teil4DF$Phase == 0, ]
# Nullmodell (nur zufällige Intercepts)
nullModelGrp11 <- glmer(TargetSlide.ACC ~ (1|Item_ID) + (1|Subject),
                        control = glmerControl(optimizer="bobyqa"),
                        family = binomial,
                        data = group11)
# Modell 1 (Nullmodell + neuer Faktor als fester Effekt)
model1Grp11 <- glmer(TargetSlide.ACC ~ trialFactor + (1|Item_ID) + (1|Subject),
                     control = glmerControl(optimizer="bobyqa"),
                     family = binomial,
                     data = group11)
# LRT
anova(nullModelGrp11,model1Grp11, test='Chisq')

# Gruppe 4: case, post
group12 <- teil4DF[teil4DF$PrimeType == 0.5 & teil4DF$Phase == 0.5, ]
# Nullmodell (nur zufällige Intercepts)
nullModelGrp12 <- glmer(TargetSlide.ACC ~ (1|Item_ID) + (1|Subject),
                        control = glmerControl(optimizer="bobyqa"),
                        family = binomial,
                        data = group12)
# Modell 1 (Nullmodell + neuer Faktor als fester Effekt)
model1Grp12 <- glmer(TargetSlide.ACC ~ trialFactor + (1|Item_ID) + (1|Subject),
                     control = glmerControl(optimizer="bobyqa"),
                     family = binomial,
                     data = group12)
# LRT
anova(nullModelGrp12,model1Grp12, test='Chisq')

###########################
## Abbildung 5 nachbauen ##
###########################

## Als letzten Schritt solltet ihr Abbildung 5 (S. 250) nachbauen
## (abzüglich der Säulen zum Primetyp "NP1 overlap").
##
## Notiert dafür zuerst, was auf der y-Achse und was auf der x-Achse im Einzelnen 
## abgetragen ist und was durch die Grautöne kodiert wird. 
## Erstellt die Abbildung mit ggplot2. Im Moodle-Kurs Statistische Grundlagen 1 habt Ihr dazu 
## letztes Semester eine Lektion bearbeitet. Eine weitere, optionale Lektion findet Ihr in diesem 
## Kurs als Zusatzlektion: https://moodle.ruhr-uni-bochum.de/course/view.php?id=50342&section=14
?geom_bar
?geom_errorbar
aggrDF <- df[df$prime_target == 2, ]
aggrData <- aggregate(TargetSlide.ACC ~ AgeGroup + Phase, data = aggrDF, FUN = sum)

ggplot() +
geom_bar(data = aggrData, aes(x=AgeGroup + Phase, y=TargetSlide.ACC), stat = 'identity') +
geom_errorbar(data = aggrData, aes(x=AgeGroup + Phase, ymin=TargetSlide.ACC-sd(TargetSlide.ACC), ymax=TargetSlide.ACC+sd(TargetSlide.ACC)), width=0.4, colour="orange", alpha=0.9)
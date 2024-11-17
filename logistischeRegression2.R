##### Laden der benoetigten Pakete
install.packages("naniar")
library(naniar)
install.packages("dplyr")
library(dplyr)
library(descr)    # Pseudo R2
library(lmtest)   # LR Test
library(car)      # VIF
library(readxl)   # einlesen der Daten
library(haven)

########Datenvorbereitung
GLES <- read_sav("C:/Users/Emma/Desktop/semester 3/statistik/ZA7716_main_v1-0-0.sav")
#GLES <- read_sav("C:/Users/emmaa/Desktop/HSW/Sem3/StatistikII/projekt/ZA7716_main_v1-0-0 (2).sav")
View(GLES)

#Spalten auswählen, rest raus werfen
dataChoosed <- data.frame(
  GLES$t1,
  GLES$t5,
  GLES$t6,
  GLES$t21,
  GLES$t32,
  GLES$t366, 
  GLES$t443,
  GLES$t38,
  GLES$t1023a,
  GLES$t1023e,
  GLES$t1023c,
  GLES$t1023i,
  GLES$t1023b,
  GLES$t1023n,
  GLES$t1023l,
  GLES$t4,
  GLES$t1026,
  GLES$t70)

#ausgewählte Spalten umbenennen  
colnames(dataChoosed) <- c(
  "Geschlecht",
  "politisches Interesse",
  "demokratie Zufriedenheit",
  "eigene aktuelle wirtschaftliche Lage",
  "Wahlbeteiltigung Bundesttagswahl 2021",
  "Wahlbeteiltigung Europawahl 2024",
  "Wahlbeteiltigung Bekanntenkreis Bundestagswahl",
  "Print: Nutzung, politisch, Wochenzeitschriften",
  "Soziale Medien: Facebook",
  "Soziale Medien: WhatsApp",
  "Soziale Medien: Instagram",
  "Soziale Medien: YouTube",
  "Soziale Medien: X",
  "Soziale Medien: TikTok",
  "Soziale Medien: Telegram",
  "Bundesland",
  "Staatsbürgerschaft deutsch",
  "Haushaltsnettoeinkommen")

# Entferne Zeilen mit NA-Werten
dataChoosed <- na.omit(dataChoosed)
View(dataChoosed)

colSums(dataChoosed == -99 | dataChoosed == -98 | dataChoosed == -97 | dataChoosed == -93)

invalid_values <- c(-99, -98, -97, -93)

dataChoosed1 <- dataChoosed[!apply(dataChoosed[, c(
  "Geschlecht",
  "politisches Interesse",
  "demokratie Zufriedenheit",
  "eigene aktuelle wirtschaftliche Lage",
  "Wahlbeteiltigung Bundesttagswahl 2021",
  "Wahlbeteiltigung Europawahl 2024",
  "Wahlbeteiltigung Bekanntenkreis Bundestagswahl",
  "Print: Nutzung, politisch, Wochenzeitschriften",
  "Soziale Medien: Facebook",
  "Soziale Medien: WhatsApp",
  "Soziale Medien: Instagram",
  "Soziale Medien: YouTube",
  "Soziale Medien: X",
  "Soziale Medien: TikTok",
  "Soziale Medien: Telegram",
  "Bundesland",
  "Staatsbürgerschaft deutsch",
  "Haushaltsnettoeinkommen")], 1, function(row) any(row %in% invalid_values)),]


View(dataChoosed1)

#Untersuchen, ob alle beteiltigten Spalten die gleichen Längen haben
length(dataChoosed1$`politisches Interesse`)
length(dataChoosed1$`Wahlbeteiltigung Europawahl 2024`)
length(dataChoosed1$Geschlecht)
length(dataChoosed1$`Print: Nutzung, politisch, Wochenzeitschriften`)
length(dataChoosed1$`demokratie Zufriedenheit`)
length(dataChoosed1$`eigene aktuelle wirtschaftliche Lage`)
length(dataChoosed1$`Wahlbeteiltigung Bekanntenkreis Bundestagswahl`)
length(dataChoosed1$Bundesland)
length(dataChoosed1$Haushaltsnettoeinkommen)
nrow(dataChoosed1)




attach(dataChoosed1)

#########################################################################
#Wahlbeteiltigung in "nein" und "ja" umwandeln -> WahlEU
#Wahlbeteiltigung in binär umcodieren und faktorisieren

# 1. Überprüfe die einzigartigen Werte in der Spalte
unique(dataChoosed1$`Wahlbeteiltigung Europawahl 2024`)

# 2. Wenn die Werte numerisch sind (1 = ja, 0 = nein), dann in "ja" und "nein" umwandeln
dataChoosed1$`Wahlbeteiltigung Europawahl 2024` <- ifelse(
  dataChoosed1$`Wahlbeteiltigung Europawahl 2024` == 1, "ja", "nein")

# 3. Fehlende Werte (NA) durch "nein" ersetzen
dataChoosed1$`Wahlbeteiltigung Europawahl 2024`[is.na(dataChoosed1$`Wahlbeteiltigung Europawahl 2024`)] <- "nein"

# 4. WahlEU in binär umwandeln (1 = "ja", 0 = "nein")
WahlEU_bin <- ifelse(dataChoosed1$`Wahlbeteiltigung Europawahl 2024` == "ja", 1, 0)

# 5. WahlEU in einen Faktor umwandeln
WahlEU <- factor(dataChoosed1$`Wahlbeteiltigung Europawahl 2024`, levels = c("nein", "ja"))

# Überprüfen der Ergebnisse
summary(WahlEU)        # Zeigt die Häufigkeit der Levels für den Faktor
table(WahlEU_bin)


#Spalte Geschlecht umcodieren
unique(dataChoosed1$Geschlecht)
dataChoosed1$Geschlecht <- ifelse(
  dataChoosed1$Geschlecht == 1, "weiblich", "männlich")
dataChoosed1$Geschlecht[is.na(dataChoosed1$Geschlecht)] <- "männlich"
Geschlecht_bin <- ifelse(dataChoosed1$Geschlecht == "weiblich", 1, 0)
Geschlecht <- factor(dataChoosed1$Geschlecht, levels = c("männlich", "weiblich"))
# Überprüfen der Ergebnisse
summary(Geschlecht)        # Zeigt die Häufigkeit der Levels für den Faktor
table(Geschlecht_bin)


#politisches Interesse mit 0 1 2 3 4 5 auf der x skala skalieren
dataChoosed1$`politisches Interesse` <- factor(
  dataChoosed1$`politisches Interesse`,
  levels = c(0, 1, 2, 3, 4, 5),
  ordered = TRUE)

# Überprüfen der Ergebnisse
summary(WahlEU)
summary(dataChoosed1$`politisches Interesse`)

#################################################

# Streudiagramm und Boxplotts nebeneinander erzeugen
# Versuch es erneut mit plot
plot(dataChoosed1$`politisches Interesse`,
     WahlEU,
     ylab= "Gewählt Ja / Nein", 
     xlab = "politisches Interesse")


par(mfrow=c(1,1))  # wieder ein Bild pro Plot
boxplot(dataChoosed1$`politisches Interesse` ~ WahlEU, 
        xlab="politsches Interesse", 
        horizontal = TRUE)


library(ggplot2)
ggplot(dataChoosed1, aes(x = `politisches Interesse`, y = WahlEU_bin)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) +
  labs(x = "Politisches Interesse", y = "Wahrscheinlichkeit der Wahlbeteiligung")



# Lineare Regression liefert:

dataChoosed1_linear <- lm(`politisches Interesse` ~ WahlEU_bin, family=binomial)
coefficients(summary(dataChoosed1_linear))
#Estimate Std. Error  t value  Pr(>|t|)
#(Intercept)                         2.4802495 0.03363752 73.73462 0.0000000
#`Wahlbeteiltigung Europawahl 2024`2 0.1141561 0.09350548  1.22085 0.2224039

plot(`politisches Interesse`,
     WahlEU,      ylab= "Gewählt Ja / Nein", 
     xlab = "politisches Interesse")
abline(0.17,0.018)

#Fazit: kein signifikanter linearer Zusammenhang


######################Hypothese 1############################################### 
#Wahrscheinlichkeit für Wahlbeteiltigung von politischem Interesse abhängig
dataChoosed1_logreg <- glm(WahlEU ~ `politisches Interesse`, family=binomial)
summary(dataChoosed1_logreg)
coefficients(summary(dataChoosed1_logreg))
#Coefficients:
#                         Estimate Std. Error z value Pr(>|z|)    
#(Intercept)               4.0769     0.3292  12.385  < 2e-16 ***
#  `politisches Interesse`  -0.7327   0.1040  -7.044 1.87e-12 ***
#mit steigendem politischen Interesse nimmt die Wahrscheinlichkeit einer Wahlbeteiligung zu 


######################Hypothese 2############################################### 
#Wahrscheinlichkeit für Wahlbeteiltigung von demokratie Zufriedenheit abhängig
dataChoosed2_logreg <- glm(WahlEU ~ `demokratie Zufriedenheit`, family=binomial)
summary(dataChoosed2_logreg)
#Coefficients:
#                             Estimate Std. Error z value Pr(>|z|)    
#(Intercept)                 3.30053    0.32007  10.312  < 2e-16 ***
#  `demokratie Zufriedenheit` -0.38967    0.09093  -4.285 1.82e-05 ***
#mit zunehmender Demokratiezufriedenheit steigt die Wahrscheinlichkeit einer Wahlbeteiligung 


######################Hypothese 3############################################### 
#Wahrscheinlichkeit für Wahlbeteiltigung von wirtschaftlicher Lage abhängig
dataChoosed3_logreg <- glm(WahlEU ~ `eigene aktuelle wirtschaftliche Lage`, family=binomial)
summary(dataChoosed3_logreg)
#Coefficients:
#                                       Estimate Std. Error z value Pr(>|z|)    
#(Intercept)                              3.2757     0.3372   9.713  < 2e-16 ***
#  `eigene aktuelle wirtschaftliche Lage`  -0.4008     0.1022  -3.922  8.8e-05 ***
#eine bessere wirtschaftliche Lage korreliert mit einer höheren Wahlbeteiligung 


######################Hypothese 4############################################### 
#Wahrscheinlichkeit für Wahlbeteiltigung von Wahlbeteiltigung Bekannte abhängig
dataChoosed4_logreg <- glm(WahlEU ~ `Wahlbeteiltigung Bekanntenkreis Bundestagswahl`, family=binomial)
summary(dataChoosed4_logreg)
#Coefficients:
#                                                   Estimate Std. Error z value Pr(>|z|)    
#(Intercept)                                        3.8854     0.2932  13.251  < 2e-16 ***
#  `Wahlbeteiltigung Bekanntenkreis Bundestagswahl`  -0.8699     0.1199  -7.257 3.95e-13 ***
#mit höherer Wahlbeteiligung im Bekanntenkreis steigt die Wahrscheinlichkeit der eigenen Wahlbeteiligung


######################Hypothese 5############################################### 
#Wahrscheinlichkeit für Wahlbeteiltigung von Bundesland abhängig
dataChoosed5_logreg <- glm(WahlEU ~ Bundesland , family=binomial)
summary(dataChoosed5_logreg)
#Coefficients:
#             Estimate Std. Error z value Pr(>|z|)    
#(Intercept)  1.77590    0.22803   7.788 6.81e-15 ***
#  Bundesland   0.04050    0.02783   1.456    0.146  
#zeigt kaum einen Zusammenhang

######################Hypothese 6############################################### 
#Wahrscheinlichkeit für Wahlbeteiltigung von Geschlecht abhängig
dataChoosed6_logreg <- glm(WahlEU ~ Geschlecht , family=binomial)
summary(dataChoosed6_logreg)
#Coefficients:
#                     Estimate Std. Error z value Pr(>|z|)    
#(Intercept)          1.9852     0.1413  14.054   <2e-16 ***
#  Geschlechtweiblich   0.1980     0.2077   0.953     0.34   
#zeigt nur einen sehr schwachen Zusammenhang zwischen dem Geschlecht (weiblich) und der Wahlbeteiligun.

######################Hypothese 7############################################### 
#Wahrscheinlichkeit für Wahlbeteiltigung von Print abhängig
dataChoosed7_logreg <- glm(WahlEU ~ `Print: Nutzung, politisch, Wochenzeitschriften` , family=binomial)
summary(dataChoosed7_logreg)
#Coefficients:
#                                                   Estimate Std. Error z value Pr(>|z|)    
#(Intercept)                                        4.2782     0.5587   7.657 1.90e-14 ***
#  `Print: Nutzung, politisch, Wochenzeitschriften`  -1.2405     0.2961  -4.190 2.79e-05 ***
#die Nutzung politischer Wochenzeitschriften steigert die Wahlbeteiltigung

######################Hypothese 8############################################### 
#Wahrscheinlichkeit für Wahlbeteiltigung von sozialen Medien TikTok abhängig
dataChoosed8_logreg <- glm(WahlEU ~ `Soziale Medien: Instagram` , family=binomial)
summary(dataChoosed8_logreg)
#zeigt keinen Zusammenhang



###weitere Berechnugne zu Hypothese 1
# Berechnetete Werte d1fit zuordnen
d1fit <- fitted(dataChoosed1_logreg)


# Bild aus berechnetet Werten und originalen Werten erzeugen
plot(`politisches Interesse`, d1fit, ylab='Prognostizierte Wahrscheinlichkeiten')
points(`politisches Interesse`, WahlEU, pch=16)


# Verschiedene Trennwerte p in Klassifizierungstabelle anzeigen

datafit1_p_0.3 <- fitted(dataChoosed1_logreg)>0.3
datafit1_p_0.5 <- fitted(dataChoosed1_logreg)>0.5
datafit1_p_0.7 <- fitted(dataChoosed1_logreg)>0.7

table(WahlEU, datafit1_p_0.3)
table(WahlEU, datafit1_p_0.5)
table(WahlEU,datafit1_p_0.7)


### Effektkoeffizienten
exp(dataChoosed1_logreg$coef)
# -> Die Chance steigt um den Faktor 1.1

# um die Guete des Models zu bestimmen LR-Test anwenden

### Nullmodel schaetzen
dataChoosed1_0 <- glm(WahlEU ~ 1, family = binomial)

### Ergebnisse 
summary(dataChoosed1_0)


### Likelihood-Ratio Test
lrtest(dataChoosed1_0, dataChoosed1_logreg)

# Nullhypothese: Reduziertes Modell ist wahr/besser 
# Ist der p-Wert kleiner als das vorgegebene Signifikanzniveau, kann die Nullhypothese 
# verworfen werden.
# Log-Likelihood -> je hoeher umso besser
# Perfektes Modell  -> Deviance wird 0




###
#
# Wie veraendert sich das Modell, wenn weitere Faktoren hinzu genommen werden?
#
###

# Das Geschlecht, die Kosten und die Anzahl der Umstiege soll noch mit beruecksichtigt werden
dataChoosed1_4 <- glm(WahlEU ~ `politisches Interesse` + 
                        `Wahlbeteiltigung Bekanntenkreis Bundestagswahl` + 
                        + `demokratie Zufriedenheit`
                      + `Print: Nutzung, politisch, Wochenzeitschriften`,
                      family = binomial)

summary(dataChoosed1_4)

###
#
# Da der p-Wert bei genderFrau sehr klein ist, kann man das Modell  um diese Spalte reduzieren
# Der Befehl .~. steht fuer "nehme alles wie vorher..."
###

dataChoosed1_3 <- update(dataChoosed1_4, .~. -`Print: Nutzung, politisch, Wochenzeitschriften`)
summary(dataChoosed1_3)

### welches Modell ist besser?

### LR-Test (alternative 1)
lrtest(dataChoosed1_3,dataChoosed1_4)

####
#
# Ergebnis:
#Model 1: WahlEU ~ `politisches Interesse` + `Wahlbeteiltigung Bekanntenkreis Bundestagswahl` + 
#  `demokratie Zufriedenheit`
#Model 2: WahlEU ~ `politisches Interesse` + `Wahlbeteiltigung Bekanntenkreis Bundestagswahl` + 
#  +`demokratie Zufriedenheit` + `Print: Nutzung, politisch, Wochenzeitschriften`
#Df  LogLik Df  Chisq Pr(>Chisq)  
#1   4 -281.97                       
#2   5 -279.98  1 3.9844    0.04592 *
#
####

### Anova (alternative 2)
anova(dataChoosed1_3,dataChoosed1_4, test='Chisq')


### Klassifizierung fuer p=0.5
WahlEU3_p_0.5 <- fitted(dataChoosed1_3)>0.5
table(WahlEU, WahlEU3_p_0.5)
WahlEU4_p_0.5 <- fitted(dataChoosed1_4)>0.5
table(WahlEU, WahlEU4_p_0.5)

exp(coef((dataChoosed1_3)))

#### Spalten wieder zu dataChoosed zusammenf?hren
detach(dataChoosed1)


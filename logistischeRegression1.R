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
GLES <- read_sav("C:/Users/Emma/Desktop/semester 3/statistik/ZA7716_main_v1-0-0.sav")
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
  GLES$t1057a,
  GLES$t1057e,
  GLES$t1057c,
  GLES$t1057i,
  GLES$t1057b,
  GLES$t1057n,
  GLES$t1057l,
  GLES$t4,
  GLES$t1026,
  GLES$t66,
  GLES$t73,
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
  "Soziale Medien politisch: Facebook",
  "Soziale Medien politisch: WhatsApp",
  "Soziale Medien politisch: Instagram",
  "Soziale Medien politisch: YouTube",
  "Soziale Medien politisch: X",
  "Soziale Medien politisch: TikTok",
  "Soziale Medien politisch: Telegram",
  "Bundesland",
  "Staatsbürgerschaft deutsch",
  "Geburtsland Deutschland",
  "Geburtsland Ausland",
  "Haushaltsnettoeinkommen")


# Entferne Zeilen mit NA-Werten
dataChoosed <- na.omit(dataChoosed)

View(dataChoosed)


invalid_values <- c(-99, -98, -97, -93)

dataChoosed1 <- dataChoosed[!apply(dataChoosed[, c(
  "Wahlbeteiltigung Europawahl 2024",
  "politisches Interesse",
  "Print: Nutzung, politisch, Wochenzeitschriften",
  "Wahlbeteiltigung Bekanntenkreis Bundestagswahl",
  "demokratie Zufriedenheit",
  "eigene aktuelle wirtschaftliche Lage",
  "Soziale Medien politisch: Facebook",
  "Soziale Medien politisch: WhatsApp",
  "Soziale Medien politisch: Instagram",
  "Soziale Medien politisch: YouTube",
  "Soziale Medien politisch: TikTok",
  "Soziale Medien politisch: Telegram",
  "Soziale Medien politisch: X",
  "Bundesland",
  "Haushaltsnettoeinkommen")], 1, function(row) any(row %in% invalid_values)), ]

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
length(dataChoosed1$`Soziale Medien politisch: Facebook`)
length(dataChoosed1$`Soziale Medien politisch: WhatsApp`)
length(dataChoosed1$Haushaltsnettoeinkommen)
nrow(dataChoosed1)

### Zuordnen der neuen Codierung für das Haushaltsnettoeinkommen als metrische Variable
#data <- data %>%
#  mutate(Einkommen_Beschreibung = case_when(
#    Einkommen == 1  ~ "unter 500 Euro",
#    Einkommen == 2  ~ "500 bis unter 750 Euro",
#    Einkommen == 3  ~ "750 bis unter 1000 Euro",
#    Einkommen == 4  ~ "1000 bis unter 1250 Euro",
#    Einkommen == 5  ~ "1250 bis unter 1500 Euro",
##    Einkommen == 6  ~ "1500 bis unter 2000 Euro",
#    Einkommen == 7  ~ "2000 bis unter 2500 Euro",
#    Einkommen == 8  ~ "2500 bis unter 3000 Euro",
#    Einkommen == 9  ~ "3000 bis unter 4000 Euro",
#    Einkommen == 10 ~ "4000 bis unter 5000 Euro",
#    Einkommen == 11 ~ "5000 bis unter 7500 Euro",
#    Einkommen == 12 ~ "7500 Euro bis unter 10000 Euro",
#    Einkommen == 13 ~ "10000 Euro und mehr",
#   TRUE ~ NA_character_ # Fallback für unerwartete Werte
 # ))

  # eine weitere Moeglichkeit mit Tabellen zu arbeiten und um direkt auf die Spalten einer Tabelle zuzugreifen 
  # ist der Befehl:   !ACHTUNG: immer mit detach(Tabelle) beenden!
  attach(dataChoosed1)

### Aus der ersten Spalte "Wahlbeteiltigung Europawahl" wird ein Faktor WB-EU erzeugt
dataChoosed1$`Wahlbeteiltigung Europawahl 2024` <- ifelse(dataChoosed1$`Wahlbeteiltigung Europawahl 2024`
                                                          == "ja", 1, 0)
levels(`Wahlbeteiltigung Europawahl 2024`) <- c("0","1")
`Wahlbeteiltigung Europawahl 2024` <- as_factor(`Wahlbeteiltigung Europawahl 2024`)
WahlEU <- factor(dataChoosed1$`Wahlbeteiltigung Europawahl 2024`)
# levels setzt neue Kategorien (es wurde gewäht, ja=1 und nein=0)
levels(WahlEU) <- c("nein","ja")
`Wahlbeteiltigung Europawahl 2024`
WahlEU

# Konvertiere 'politisches Interesse' in einen geordneten Faktor
dataChoosed1$`politisches Interesse` <- factor(dataChoosed1$`politisches Interesse`, 
                                               levels = c(0, 1, 2, 3, 4, 5), 
                                               ordered = TRUE)


# Streudiagramm und Boxplotts nebeneinander erzeugen
par(mfrow=c(1,2))
plot(dataChoosed1$`politisches Interesse`,
     dataChoosed1$`Wahlbeteiltigung Europawahl 2024`,
     ylab= "Gewählt Ja / Nein", 
     xlab = "politisches Interesse")

par(mfrow=c(1,1))  # wieder ein Bild pro Plot
boxplot(dataChoosed1$`politisches Interesse` ~ dataChoosed1$WahlEU, 
        xlab="politsches Interesse", 
        horizontal = TRUE)


# Achsenbeschriftungen manuell hinzufügen
axis(1, at = 1:6, labels = c("0", "1", "2", "3", "4", "5"))

library(ggplot2)
ggplot(dataChoosed1, aes(x = `politisches Interesse`, y = as.numeric(`Wahlbeteiltigung Europawahl 2024`))) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE) +
  labs(x = "Politisches Interesse", y = "Wahrscheinlichkeit der Wahlbeteiligung")



# Lineare Regression liefert:

dataChoosed1_linear <- lm(`politisches Interesse` ~ `Wahlbeteiltigung Europawahl 2024`, family=binomial)
coefficients(summary(dataChoosed1_linear))
#Estimate Std. Error  t value  Pr(>|t|)
#(Intercept)                         2.4802495 0.03363752 73.73462 0.0000000
#`Wahlbeteiltigung Europawahl 2024`2 0.1141561 0.09350548  1.22085 0.2224039

plot(`politisches Interesse`,
     `Wahlbeteiltigung Europawahl 2024`,      ylab= "Gewählt Ja / Nein", 
     xlab = "politisches Interesse")
abline(0.17,0.018)

#Fazit: kein signifikanter linearer Zusammenhang


######################Hypothese 1############################################### 
#Wahrscheinlichkeit für Wahlbeteiltigung von politischem Interesse abhängig

# Logistische Regression fuer Wahlbeteiltigung und politischem Interesse
# Wie Wahrscheinlich ist es, dass gewählt wurde bei einem politischen Interesse


dataChoosed1_logreg <- glm(`Wahlbeteiltigung Europawahl 2024` ~ `politisches Interesse`, family=binomial)
summary(dataChoosed1_logreg)
#Coefficients:
#Estimate Std. Error z value Pr(>|z|)    
#(Intercept)             -1.925006   0.111129 -17.322   <2e-16 ***
#  `politisches Interesse`  0.007979   0.027327   0.292     0.77    
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#(Dispersion parameter for binomial family taken to be 1)
#Null deviance: 851.44  on 1104  degrees of freedom
#Residual deviance: 849.96  on 1103  degrees of freedom
#AIC: 853.96

#Number of Fisher Scoring iterations: 4
coefficients(summary(dataChoosed1_logreg))
#                          Estimate Std. Error   z value     Pr(>|z|)
#(Intercept)             -2.1686352 0.23637065 -9.174723 4.527379e-20
#`politisches Interesse`  0.1034574 0.08479855  1.220037 2.224507e-01

######################Hypothese 2############################################### 
#Wahrscheinlichkeit für Wahlbeteiltigung von demokratie Zufriedenheit abhängig
dataChoosed2_logreg <- glm(`Wahlbeteiltigung Europawahl 2024` ~ `demokratie Zufriedenheit`, family=binomial)
summary(dataChoosed2_logreg)


######################Hypothese 3############################################### 
#Wahrscheinlichkeit für Wahlbeteiltigung von wirtschaftlicher Lage abhängig
dataChoosed3_logreg <- glm(`Wahlbeteiltigung Europawahl 2024` ~ `eigene aktuelle wirtschaftliche Lage`, family=binomial)
summary(dataChoosed3_logreg)

######################Hypothese 4############################################### 
#Wahrscheinlichkeit für Wahlbeteiltigung von Wahlbeteiltigung Bekannte abhängig
dataChoosed4_logreg <- glm(`Wahlbeteiltigung Europawahl 2024` ~ `Wahlbeteiltigung Bekanntenkreis Bundestagswahl`, family=binomial)
summary(dataChoosed4_logreg)

######################Hypothese 5############################################### 
#Wahrscheinlichkeit für Wahlbeteiltigung von Bundesland abhängig
dataChoosed5_logreg <- glm(`Wahlbeteiltigung Europawahl 2024` ~ Bundesland , family=binomial)
summary(dataChoosed5_logreg)

######################Hypothese 6############################################### 
#Wahrscheinlichkeit für Wahlbeteiltigung von Geschlecht abhängig
dataChoosed6_logreg <- glm(`Wahlbeteiltigung Europawahl 2024` ~ Geschlecht , family=binomial)
summary(dataChoosed6_logreg)

######################Hypothese 7############################################### 
#Wahrscheinlichkeit für Wahlbeteiltigung von Print abhängig
dataChoosed7_logreg <- glm(`Wahlbeteiltigung Europawahl 2024` ~ `Print: Nutzung, politisch, Wochenzeitschriften` , family=binomial)
summary(dataChoosed7_logreg)

######################Hypothese 8############################################### 
#Wahrscheinlichkeit für Wahlbeteiltigung von Print abhängig
dataChoosed8_logreg <- glm(`Wahlbeteiltigung Europawahl 2024` ~ `Soziale Medien politisch: WhatsApp` , family=binomial)
summary(dataChoosed8_logreg)



###weitere Berechnugne zu Hypothese 1
# Berechnetete Werte d1fit zuordnen
d1fit <- fitted(dataChoosed1_logreg)


# Bild aus berechnetet Werten und originalen Werten erzeugen
plot(`politisches Interesse`, d1fit, ylab='Prognostizierte Wahrscheinlichkeiten')
points(`politisches Interesse`, `Wahlbeteiltigung Europawahl 2024`, pch=16)

  
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
dataChoosed1$Geschlecht <- ifelse(gender == "ja", 1, 0)
levels(dataChoosed1$Geschlecht) <- c("0","1")
gender <- factor(Geschlecht)
levels(gender) <- c("Mann", "Frau")
summary(gender)
dataChoosed1_4 <- glm(WahlEU ~ `politisches Interesse` + 
                       `Wahlbeteiltigung Bekanntenkreis Bundestagswahl` + 
                         + `demokratie Zufriedenheit`
                        + `Print: Nutzung, politisch, Wochenzeitschriften`,
                   family = binomial)

summary(dataChoosed1_4)

###
#
# Da der p-Wert bei genderFrau sehr klein ist, kann man das Modell 
# um diese Spalte reduzieren
#
# Der Befehl .~. steht fuer "nehme alles wie vorher..."
###

dataChoosed1_3 <- update(dataChoosed1_4, .~. -gender)
summary(dataChoosed1_3)

### welches Modell ist besser?

### LR-Test (alternative 1)
lrtest(dataChoosed1_3,dataChoosed1_4)

####
#
# Ergebnis:
#Model 1: WahlEU ~ `politisches Interesse` + `Wahlbeteiltigung Bekanntenkreis Bundestagswashl` + 
#  `Print: Nutzung, politisch, Wochenzeitschriften`
#Model 2: WahlEU ~ `politisches Interesse` + `Wahlbeteiltigung Bekanntenkreis Bundestagswashl` + 
#  gender + `Print: Nutzung, politisch, Wochenzeitschriften`
#Df  LogLik Df  Chisq Pr(>Chisq)
#1   4 -423.69                     
#2   5 -423.66  1 0.0553     0.8141
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

    
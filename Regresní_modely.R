#Lineární regrese
data <- Ichs #<- zdravotnické data
data

# Zavislost hmotnosti na vysce
hmot <- data$hmot
vyska <- data$vyska
# Vizualizace
#==> rostouci trend, kdyz roste vyska, tak i vaha
plot(hmot~vyska,pch=19)

# Model lin. regrese
model <- lm(hmot~vyska)
coef(model)
#==> předpis rovnice
# hmotnost = -66.849 + 0.845*vyska
#==> jeden centimetr vysky prida 0.845 Kg

summary(model)
# Adjussted R-squared:  0.2493 -> vysvětluje cca 25% variability dat
# Stredni chyba residui - Residual standard error: 9.222

"""
Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) -66.8494    18.1990  -3.673 0.000307 *** #<--- vyznamnosti regresnich koef.
vyska         0.8455     0.1032   8.190 2.93e-14 *** #<--- vyznamnosti regresnich koef.
"""
# Akaikeho a Bayesovske kriterium
#- pomahi vybrat model, který nejlépe vyvětluje data při zohlednění počtu parametru
#- najít rovnováhu mezi složitostí modelu a jeho schopností dobře popsat data.
AIC(model)
BIC(model)


#----------------------------------------------------------

#Testy předpokladů
# 1. graficky
par(mfrow=c(2,2))
plot(model)
par(mfrow=c(1,1))

# 1. Residuals - nesmí mít trend
# 2. Má být na přímce - normalita
# 3. Stabilita rozptylu - nema mit trend
# 4. Vlivnost pozorování - zadny bod nesmi prekrocovat mez (vzdálenost)

# 2. test normality residuí
# H0: maji residua normalni rozdeleni | H1: NEmaji residua norm. rozd...
shapiro.test(residuals(model))
# p-value = 0.0149 < alfa ->zamítám H0 => Nemají residua normální rozdělení

library(lmtest)

# 3. Test stability rozptylu
# H0: rozptyly jsou shodne | H1: NEjsou shodne rozptyly..
bptest(model)
# p-value = 0.4871 > alfa -> nezamítáme H0 => jsou shodné

# 4. Cookova vzdálenost
"""
Hodnota Cookovy vzdálenosti měří vliv, který má odstranění daného pozorování na 
odhadované parametry modelu. Vyšší hodnoty naznačují, že odstranění pozorování má 
významnější vliv na odhady parametrů.

Často se používá pravidlo, že pozorování s Cookovou vzdáleností vyšší než 1 může být považováno za vlivné. 
Toto číslo však není pevné a může se lišit v závislosti na kontextu analýzy a na množství dostupných dat.
"""
cooks.distance(model)
# Největší hodnota cookovy vzdálenosti
cooks.distance(model)[which.max(cooks.distance(model))]

# 5. Vlivnost pozorování
influence.measures(model)
summary(influence.measures(model))


# Vícenásobná regrese
# Zavislost hmotnosti na systolickem a diastolickem tlaku

hmot <- data$hmot
syst <- data$syst
diast<-data$diast

plot(hmot~syst, pch=19, main="Hmot X Syst")
plot(hmot~diast,pch=19, main="Hmot X Diast")

model <- lm(hmot~syst+diast)
summary(model)
# hmot = 57.71485 + 0.14924 * syst + 0.05462 * diast
# Syst je v modelu významnější
# R squared: 0.07405 -> dost chabé -> vysvětluji modelem 7,5% variability dat

#coef(model) <- coefy pro odhad regresní rovnice

# Předpoklady
par(mfrow=c(2,2))
plot(model)
par(mfrow=c(1,1))

# 1.graf : trend dolu u residuí -> špatně (ale jedno vlivné pozorování)
# 2.graf : leží na diagonále - OK
# 3.graf : vidím trend u rozptylu
# 4.graf : u vlivnosti vidím přesahující hodnoty Cookovy vzdálenosti - špatně

# Ze summary modelu vyšla váznamná závislo hmot x syst, ale z grafu je viditelná závislost i na diast

model2 <- lm(hmot~diast)
summary(model2)

"""
Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) 62.94712    5.27110  11.942  < 2e-16 ***
diast        0.22608    0.06159   3.671  0.00031 ***
#===> taky významně závisí
"""

# Závislost dias a syst
cor(syst,diast)
#0.7970539 => významná korelace


#----------------------------------------------------------

## Kategoricka promenna v linearnim modelu
# Závisí hmotnost na kategorické prom kouření?

smoke <- data$Kour
hmot <- data$hmot

model3 <- lm(hmot~smoke)
boxplot(hmot~smoke) #-> spíše nesouvisí
summary(model3)
#=> nic kromě interceptu s hmotností nesouvisí - viz. graf
# Adjusted R-squared:  0.002598 

# Anova
#H0 <- hmotnost se NElisi vuci skupinam | H1 <- hmotnost se lisi vuci skupinam
anova(model3)
# p-value = 0.3199 > alfa -> nezamítáme H0 - koureni a hmotnot spolu nesouvisi


#----------------------------------------------------------
#ANOVA - dvojne trideni - 2x kategoricka prom
#==> pouziti Anova() => z balíčku car, RcmdrMisc - plotMeans()

# Jak hmotnost zavisi na koureni a cholesterolu?
library(RcmdrMisc)
library(car)
Cholest<-Ichs$Cholest
plotMeans(hmot, smoke, Cholest, error.bars="se", connect=TRUE,legend.pos="farright")
plotMeans(hmot, smoke, Cholest, error.bars="conf.int", connect=TRUE,legend.pos="farright")
Anova(aov(hmot~smoke*Cholest))

# ANOVA pres regresni model
summary(lm(hmot~smoke*Cholest))
Anova(lm(hmot~smoke*Cholest))

# hmotnost se v zavislosti na jednotlivych promennych nelisi
Anova(lm(hmot~smoke))
plot(hmot~smoke)
Anova(aov(hmot~Cholest))
plot(hmot~Cholest)

#----------------------------------------------------------

# Vícenásobná lineární regrese

"""
Výběr mezi použitím + a * závisí na vaší hypotéze o tom, zda interakce mezi proměnnými 
mají významný vliv na závislou proměnnou. 

Je důležité zvážit teoretický základ vaší analýzy při rozhodování o struktuře modelu.

"""

# Hledani optimalniho modelu
# Jak cholesterol zavisi na koureni,hmotnosti a vysce?

hmot <- hmot
syst <- syst
kour <- smoke
vyska <- data$vyska

m <- lm(syst~kour*hmot*vyska)

summary(m)
# Adjusted R-squared:  0.1514 

#Kroková regrese
#=> iterativně vytvařím modely, zjistím AIC a měním regresory, ten co má nejnižší AIC je nejoptimálnější
step(lm(syst~kour*hmot*vyska))

#Optimální model
opt_m <- lm(syst ~ kour * hmot * vyska)
summary(opt_m)
# Adjusted R-squared:  0.1514 


#----------------------------------------------------------

#### Samostatne
# Uvazujte data mtcars
data(mtcars)

# Na cem zavisi sila vozu, promenna hp? Promenne cyl, vs, am a gear uvazujte jako kategoricke.
# Jaky je rozdil mezi automatickou a manualni prevodovkou (promenna am)?
# Je dulezita interakce vs a disp? A co interakce vs a am? A interakce am a disp?
# A co kdyz pridam jeste interakce vs a am s mpg?
# Kolik procent variability se vyslednym modelem vysvetli? 
# Jsou splneny predpoklady?
# Spoctete predpoved ...

#----------------------------------------------------------

#### Logisticka regrese
library(datarium)
library(car)

smpl<-sample(1:2201,150)
titanic<-titanic.raw[smpl,]
titanic

dependent <- titanic$Survived=="Yes"
dependent

# Náhled nad daty
table(titanic$Survived,titanic$Class)
table(titanic$Survived,titanic$Age)
table(titanic$Survived,titanic$Sex)

# Závislost přeživších na jejich věkové skupině, třídě v lodi a pohlaví
m1 <- glm(dependent~titanic$Class+titanic$Age+titanic$Sex, family="binomial")
summary(m1)

library(fmsb)

# Vypočet R2
NagelkerkeR2(m1)

Anova(m1)

b <- coef(m1)
b

1/exp(b[3])
# kolikrat se zvysuji sance na preziti u cestujicich v prvni tride oproti cestujicim ve treti tride
1/exp(b[4])
# kolikrat se zvysuji sance na preziti u cestujicich v prvni tride oproti posadce
exp(b[5])
# kolikrat se zvysuji sance na preziti u zen oproti muzum

# Jake sance na preziti ma dospely muz cestujici druhou tridou?
(odd<-exp(b[1]+b[2]+b[6]))
# A jakou ma pravdepodobnost, ze prezije
(prob<-odd/(1+odd))

#----------------------------------------------------------
#TO:DO
#Věcná významnost
#Bayesovké sítě
# Fuzzy logika, množiny + modelování
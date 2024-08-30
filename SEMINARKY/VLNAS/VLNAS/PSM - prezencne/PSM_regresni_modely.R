########################
## Jednoducha linearni regrese
# nactete data Ichs

# Zavislost hmotnosti na vysce 
hmot<-Ichs$hmot
vyska<-Ichs$vyska
plot(hmot~vyska,pch=19,main="Zavislost hmotnosti na vysce")
abline(lm(hmot~vyska),col=2)
  # regresni primka

# Model jednoduche linearni regrese
model<- lm(hmot~vyska)
coef(model)
  # odhady regresnich koeficientu: hmotnost = -66.85 + 0.84*vyska
  # na 1 cm vysky pripada v prumeru 0.84 kg hmotnosti
summary(model)
  # odhad a vyznamnost regresnich koeficientu
  # Residual standard error - stredni chyba residui
  # Multiple R-squared - koeficient determinace
  #   kolik procent variability se zavislosti vysvetli
AIC(model)
BIC(model)
  # Akaikeho a Bayesovske kriterium

### Testy predpokladu
# graficka diagnostika
par(mfrow=c(2,2))
plot(model)
par(mfrow=c(1,1))
  # 1. graf: linearita zavislosti
  #       cervena cara nema mit trend - OK
  # 2. graf: normalita residui
  #       body maji byt na primce - OK
  # 3. graf: stabilita rozptylu (homoskedasticita)
  #       cervena cara nema mit trend - OK
  # 4. graf: vlivnost pozorovani (Cook's distance)
#       zadny bod nevybocuje z mezi - OK

# test normality residui 
shapiro.test(residuals(model))
# test stability rozptylu
library(lmtest)
bptest(model)

# Cookova vzdalenost
cooks.distance(model)
cooks.distance(model)[which.max(cooks.distance(model))]
  # nejvetsi hodnota Cookovy vzdalenosti
# merime vlivnost pozorovani
influence.measures(model)
summary(influence.measures(model))

# Multikolinearita (ve vicenasobne regresi)
# Zavislost hmotnosti na systolickem a diastolickem tlaku
syst<-Ichs$syst
diast<-Ichs$diast
plot(hmot~syst,pch=19,main="Zavislost hmotnosti na systolickem tlaku")
plot(hmot~diast,pch=19,main="Zavislost hmotnosti na diastolickem tlaku")

summary(lm(hmot~diast+syst))
  # vidim jen slabou zavislost na systolickem tlaku
summary(lm(hmot~diast))
summary(lm(hmot~syst))
  # ve skutecnosti je evidentni zavislost na obou tlacich
plot(syst~diast,pch=19,main="Vztah mezi systolickym a diastolickym tlakem")
cor(syst,diast)
  # vysoka vzajemna korelace

library(car)
vif(lm(hmot~diast+syst))
  # Variance inflaction factor - ma byt maly (idealne pod 1)

######################
## Kategoricka promenna v linearnim modelu
Smok<-Ichs$Kour
plot(hmot~Smok)
lm(hmot~Smok)
summary(lm(hmot~Smok))
  # vidim vyznamnost dummy promennych
anova(lm(hmot~Smok))
  # vyznamnost cele promenne dohromady 

#####################
### Interakce v ANOVe - dvojne trideni

# Jak hmotnost zavisi na koureni a cholesterolu?
library(RcmdrMisc)
library(car)
Cholest<-Ichs$Cholest
plotMeans(hmot, Smok, Cholest, error.bars="se", connect=TRUE,legend.pos="farright")
plotMeans(hmot, Smok, Cholest, error.bars="conf.int", connect=TRUE,legend.pos="farright")
Anova(aov(hmot~Smok*Cholest))

# ANOVA pres regresni model
summary(lm(hmot~Smok*Cholest))
Anova(lm(hmot~Smok*Cholest))
  # hmotnost se v zavislosti na jednotlivych promennych nelisi
Anova(lm(hmot~Smok))
plot(hmot~Smok)
Anova(aov(hmot~Cholest))
plot(hmot~Cholest)

#######################
### Multiple linear regression

# How Systolic pressure depends on weight and Cholesterol level
plot(syst~hmot,pch=19,col=as.integer(Cholest))
abline(lm(syst[as.integer(Cholest)==1]~hmot[as.integer(Cholest)==1]),col=1)
abline(lm(syst[as.integer(Cholest)==2]~hmot[as.integer(Cholest)==2]),col=2)
summary(lm(syst~hmot*Cholest))
  # Zavislost se podle cholesterolu nelisi

plot(syst~hmot,pch=19,main="Zavislost systolickeho tlaku na hmotnosti")
abline(lm(syst~hmot),col=2,lwd=2)
  # Zavislost systolickeho tlaku na hmotnosti
confint(lm(syst~hmot))
  # intervaly spolehlivosti pro parametry modelu

# Interval spolehlivosti pro odhad
new <- data.frame(hmot = c(86, 87, 88))
predict(lm(syst~hmot),new,interval="confidence")

# predikcni interval spolehlivosti
new <- data.frame(hmot = c(86, 87, 88))
predict(lm(syst~hmot),new,interval="prediction")

# How Systolic pressure depends on weight and smoking
Kour<-Ichs$Koureni
plot(syst~hmot,pch=19,col=as.integer(Kour))
abline(lm(syst[as.integer(Kour)==1]~hmot[as.integer(Kour)==1]),col=1)
abline(lm(syst[as.integer(Kour)==2]~hmot[as.integer(Kour)==2]),col=2)

# Hledani optimalniho modelu
model1<-lm(syst~hmot*Kour*vyska)
summary(model1)
  # model se vsemi promennymi z nejz budem postupne vynechavat
  #   metoda backward (zpetna) 
  # nejvetsi inerakce neni vyznamna
model2<-lm(syst~hmot*Kour+vyska*Kour+vyska*hmot)
summary(model2)

model3<-lm(syst~hmot*Kour+vyska*Kour)
summary(model3)

model4<-lm(syst~hmot+vyska*Kour)
summary(model4)

model5<-lm(syst~hmot+vyska+Kour)
summary(model5)

model6<-lm(syst~hmot+Kour)
summary(model6)

# Coefficient of determination
summary(model1)$r.squared
summary(model2)$r.squared
summary(model3)$r.squared

# Krokova regrese
step(lm(syst~hmot*Kour*vyska))

##########################
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

##########################
#### Logisticka regrese

library(datarium)
library(car)
  # v knihovne datarium je vhodna databaze z Titaniku
smpl<-sample(1:2201,150)
titanic<-titanic.raw[smpl,]
  # vyber 150 hodnot, abychom mohli pracovat i s p-hodnotama
dependent<- (titanic$Survived=="Yes")
  # zavisle promenna

table(titanic$Survived,titanic$Class)
table(titanic$Survived,titanic$Age)
table(titanic$Survived,titanic$Sex)
  # kontrola, jaci pasazeri prezili

mod1<-glm(dependent~titanic$Class+titanic$Sex+titanic$Age,family="binomial")
  # model logisticke regrese
summary(mod1)
  # odhady koefificnetu a jejich vyznamnost

library(fmsb)
NagelkerkeR2(mod1)
  # neco jako koeficient determinace (pocitany z deviance)

Anova(mod1,type="II")
  # vyznamnost promennych

# interpretace regresnich koeficientu
(b<-coef(mod1))
  # ulozeni koeficientu do promenne
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

########################
### Samostatne

## Zavisi hladina cholesterolu na hmotnosti, koureni a na veku?
# promenne Rchol, hmot, Kour, vek

########################
### Poissonova regrese - log-linear model
# zavisle promennou tvori pocty
p <- read.csv("https://stats.idre.ucla.edu/stat/data/poisson_sim.csv")
p <- within(p, {
  prog <- factor(prog, levels=1:3, labels=c("General", "Academic", 
                                            "Vocational"))
  id <- factor(id)
})
  # pocty oceneni ziskane studenty v ruznych studijnich programech
  #   pridana je zavislost na vysledcich testu z matematiky
(tab<-table(p$prog,p$num_awards))
barplot(tab,beside=T,col=2:4,main="Numbers of awards for different study programs",legend=T)
  # graficke znazorneni

# Poissonova regrese
(mod.p1 <- glm(num_awards ~ prog + math, family="poisson", data=p))
summary(mod.p1)
library(car)
Anova(mod.p1)
  # obe nezavisle promenne maji vyznamny vliv
  #   "General" a "Vocational" program se od sebe vyznamne nelisi

confint(mod.p1)
  # intervaly spolehlivosti pro regresni koeficienty
NagelkerkeR2(mod.p1)
  # neco jako koeficient determinace

# Graficky vystup
pred<-predict(mod.p1, type="response")
vyst<-data.frame(p$math,p$prog,pred)
vyst<-vyst[order(vyst[,1]),]
plot(num_awards~math,col=prog,pch=19,data=p,main="Zavislost poctu oceneni na testu z matematiky")
lines(vyst[vyst[,2]=="General",1],vyst[vyst[,2]=="General",3],col=1)
lines(vyst[vyst[,2]=="Academic",1],vyst[vyst[,2]=="Academic",3],col=2)
lines(vyst[vyst[,2]=="Vocational",1],vyst[vyst[,2]=="Vocational",3],col=3)
legend(35,5.5,levels(p$prog),lty=1,col=1:3)


##################
### Poradova (ordinalni) regrese
library(MASS)
library(foreign)
dat <- read.dta("https://stats.idre.ucla.edu/stat/data/ologit.dta")
# example data from internet
# data: apply - how likely the student will apply for graduation on a school
#       parent - are the parents graduated?
#       public - is the undergraduate institution public or private?
#       GPA - average score on the undergraduate institution
ftable(xtabs(~ public + apply + pared, data = dat))
# summary contingency table
# a question is: on what the probability to apply for graduation depends?
m <- polr(apply ~ pared + public + gpa, data = dat, Hess=TRUE)
summary(m)
# basic outputs for estimates of regression coefficients
ctable <- coef(summary(m))
pval <- pt(abs(ctable[, "t value"]), df=length(dat$apply)-1, lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p))
# table for coefficients together with the p-values
# type of the school (public or private) is not important
#   other variables have significant impact on apply of graduation

## OR and CI
(ci <- confint(m))
# confidence intervals for linear coefficients
exp(cbind(OR = coef(m), ci))
# how many times the odds of apply of graduation increase, when
#   the independent variable increases by one, while the other independent variables stay fixed
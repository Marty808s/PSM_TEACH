##############################
## Nacteni databaze o autech
force(mtcars)

## Promenne
# mpg - vzdalenost ujeta na 1 gallon
# cyl	- pocet valcu
# disp - objem
# hp - Gross horsepower (sila)
# drat - pomer os
# wt - vaha (1000 liber)
# qsec - 1/4 mile time
# vs - typ motoru (0 = ve tvaru V, 1 = primy)
# am - prevodovka (0 = automatic, 1 = manual)
# gear - pocet prevodu
# carb - pocet karburatoru

library(DescTools)
##############################
## Dvouvyberovy t-test

# Lisi se ujeta vzdalenost v zavislosti na typu motoru?
cislo<-mtcars$mpg
kategorie<-mtcars$vs

# Normalita se testuje pro kazdou skupinu zvlast
#   H0: data maji normalni rozdeleni vs. H1: data nemaji normalni rozdeleni
par(mfrow=c(1,2))
tapply(cislo,kategorie,PlotQQ)
par(mfrow=c(1,1))
  # body lezi v obou pripadech priblizne na primce
tapply(cislo,kategorie,shapiro.test)
  # obe p-hodnoty > alfa -> nezamitame H0, 
  #   data maji priblizne normalni rozdeleni -> pouziji t-test

# nejprve graficke zobrazeni
boxplot(cislo~kategorie,main="Ujeta vzdalenost podle typu motoru",col=c(3,4))
  # vidime poradi kategorii, muze se hodit

# testujeme hypotezy
# H0: V-motor - primy motor = 0;  H1: V-motor - primy motor <> 0

# Mame na vyber dva dvouvyberove t-testy:
#   pro shodne rozptyly
#   pro ruzne rozptyly

# Jsou rozptyly shodne?
# H0: rozptyly se nelisi; H1: rozptyly se lisi
var.test(cislo~kategorie)
  # p-hodnota = 0.1997 > alfa (0.05) -> nezamitame H0
  #   rozptyly jsou priblizne shodne, pouzijeme t-test pro shodne rozptyly
t.test(cislo~kategorie,var.eq=T)
t.test(cislo~kategorie,mu=0,alternative="two.sided",var.eq=T)
# p-hodnota = 3.416e-05 < alfa (0.05) -> zamitame H0, plati H1
#   ujeta vzdalenost se podle typu motoru lisi
# Ktery motor "vice zere"?

# Spojitost s intervalem spolehlivosti pro rozdil 
MeanDiffCI(cislo~kategorie)
  # pocita pro variantu s ruznymi rozptyly

## Zavisi sila vozu na typu prevodovky?
cislo<-mtcars$hp
kategorie<-mtcars$am

# Test normality
#   H0: data maji normalni rozdeleni vs. H1: data nemaji normalni rozdeleni
par(mfrow=c(1,2))
tapply(cislo,kategorie,PlotQQ)
par(mfrow=c(1,1))
  # u manualni prevodovky je zrejma odchylka od normality
tapply(cislo,kategorie,shapiro.test)
  # jedna p-hodnota < alfa -> zamitame H0, 
  #   data nemaji normalni rozdeleni -> pouziji  Wilcoxonuv test

# nejprve graficke zobrazeni
boxplot(cislo~kategorie,main="Sila vozu podle typu prevodovky",col=c(3,4))

# testujeme hypotezy
# H0: automat - manual = 0;  H1: automat - manual <> 0

# Test shody rozptylu - i neparametricky test ocekava priblizne stejne rozptyly
# H0: rozptyly se nelisi; H1: rozptyly se lisi
var.test(cislo~kategorie)
  # p-hodnota = 0.08622 > alfa (0.05) -> nezamitame H0
  #   rozptyly jsou priblizne shodne - predpoklad splnen
wilcox.test(cislo~kategorie)
  # p-hodnota = 0.0457 < alfa (0.05) -> zamitame H0, plati H1
  #   sila vozu zavisi na typu prevodovky
  # Ktera prevodovka se poji se silnejsimi vozy?

# T-test by rozdil nevidel
  t.test(cislo~kategorie)

# Jsou vozy s motory do V silnejsi? (promenne hp, vs)
# Jsou vozy s automatickou prevodovkou tezsi? (promenne wt, am)

# pouzijte parametr alternative = "less" nebo alternative = "greater"
  
#########################################
## ANOVA - analyza rozptylu  
  
# Lisi se cas, za nejz ujedou auta 1/4 mile podle poctu valcu?
cislo<-mtcars$qsec
kategorie<-as.factor(mtcars$cyl)
  
# Normalita se testuje pro residua linearniho modelu
#   H0: data maji normalni rozdeleni vs. H1: data nemaji normalni rozdeleni
res<-residuals(lm(cislo~kategorie))
PlotQQ(res)
  # body lezi priblizne na primce - data maji priblizne normalni rozdeleni
# Kdo chce, muze i ciselny test, ale neni nutne
shapiro.test(res)
  # p-hodnota 0.1432 > alfa -> nezamitame H0, 
  #   data maji priblizne normalni rozdeleni -> pouziji parametrickou ANOVU
  
# nejprve graficke zobrazeni
boxplot(cislo~kategorie,main="Cas podle poctu valcu",col="orange")
  # cas s poctem valcu klesa
  
# testujeme hypotezy
  # H0: vsechny skupiny jsou stejne;  H1: alespon jedna skupina se lisi
  # H0: cas na poctu valcu nezavisi; H1: cas na poctu valcu zavisi
  
# Opet je treba otestovat shodu rozptylu 
  # dle vysledku se voli typ ANOVy
# Test shody rozptylu
  # H0: rozptyly se nelisi; H1: rozptyly se lisi
bartlett.test(cislo~kategorie)
  # p-hodnota = 0.4554 > alfa (0.05) -> nezamitame H0
  #   rozptyly jsou priblizne shodne, pouzijeme klasickou ANOVu
anova(aov(cislo~kategorie))
  # tabulka analyzy rozptylu
  # p-hodnota = 0.001955 < alfa (0.05) -> zamitame H0, plati H1
  #   cas se podle poctu valcu lisi

# Muze prijit doplnujici otazka: ktere dvojice skupin se od sebe vyznamne lisi?
# parove srovnani
TukeyHSD(aov(cislo~kategorie))
  # jen 8 valcu a 4 valce
plot(TukeyHSD(aov(cislo~kategorie)))  
  # vidime intervaly spolehlivosti pro rozdily

## Zavisi pomer os na poctu prevodu?
cislo<-mtcars$drat
kategorie<-as.factor(mtcars$gear)
  
# Test normality
  #   H0: data maji normalni rozdeleni vs. H1: data nemaji normalni rozdeleni
res<-residuals(lm(cislo~kategorie))
PlotQQ(res)  
  # vidime odchylku od normality
# Mozno potvrdit ciselnym testem
shapiro.test(res)
  # p-hodnota 0.001428 < alfa -> zamitame H0, 
  #   data nemaji normalni rozdeleni -> pouziji  Kruskal-Wallisuv test
  
# nejprve graficke zobrazeni
boxplot(cislo~kategorie,main="Pomer os podle poctu prevodu",col="coral")
  
# testujeme hypotezy
  # H0: pomer os na poctu prevodu nezavisi; H1: pomer os na poctu prevodu zavisi

# Test shody rozptylu - i neparametricky test ocekava priblizne stejne rozptyly
  # H0: rozptyly se nelisi; H1: rozptyly se lisi
bartlett.test(cislo~kategorie)
  # p-hodnota = 0.6596 > alfa (0.05) -> nezamitame H0
  #   rozptyly jsou priblizne shodne - predpoklad splnen
kruskal.test(cislo~kategorie)
  # p-hodnota = 2.242e-05 < alfa (0.05) -> zamitame H0, plati H1
  #   pomer os zavisi na poctu prevodu 
# Ktera dvojice skupin se od sebe vyznamne lisi?
DunnTest(cislo~kategorie)
  # Vyznamne se lisi vozy se tremi prevody od ostatnich

# Zavisi vzdalenost ujeta na 1 gallon na poctu prevodu? (promenne mpg, gear)
# Lisi vaha vozu podle poctu valcu? (promenne wt, cyl)

# Analyza rozptylu pro pripad, ze se lisi variabilita ve skupinach
oneway.test(cislo~kategorie, var.equal = FALSE)

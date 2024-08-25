library(DescTools)
library(TeachingDemos)

data <- Kojeni2

# Jednovýběrový t-test
#=> parametrická -> používá k testovánní střední hodnotu
#=> potřebuje normální rozdělení

# Na zaklade intervalu spolehlivosti rozhodnete, zda stredni hodnota
#   vysky matek muze byt 164 cm?
#   A co 165 cm, 166 cm, 167 cm, 168 cm,  169 cm, ... ?

#H0 - muze byt 164
#H1 - nemuze byt 164

prom1 <- data$vyskaM

MeanCI(prom1) #<- podle MeanCI nemůže, protože je mimo dolní ci
t.test(prom1, mu=164) #p-value = 4.923e-06, takže zamítám nulovou hypotézu -> nemůže!

# Co je interval spolehlivosti?

"""
Pokud máte vzorek dat a vypočítáte průměrnou hodnotu, můžete vytvořit interval spolehlivosti, který vám řekne, 
v jakém rozmezí se pravděpodobně nachází skutečný průměr celé populace, ze které vzorek pochází. Například pokud 
  vypočítáte, že průměrný věk ve vzorku je 30 let a interval spolehlivosti 95 % je 28 až 32 let, znamená to, že s 95% 
  pravděpodobností je skutečný průměrný věk v populaci mezi 28 a 32 lety.
"""

# Velikost vzorku
n <- length(prom1)

# Průměr
mean_value <- mean(prom1)

# Standardní odchylka
sd_value <- sd(prom1)

# Hladina významnosti (např. pro 95% interval spolehlivosti)
alpha <- 0.05

# Kritická hodnota t (pro 95% interval a n-1 stupňů volnosti)
t_value <- qt(1 - alpha/2, df = n - 1) # slouží k získání kvantilu
#- 1. parametr je úrovveň pst - 100%-5% = 95%
# - 2. je stupeˇvolnosti

# Chyba odhadu
error_margin <- t_value * (sd_value / sqrt(n))

#-> (sd_value / sqrt(n)) -> standartní chyba
#-> sqrt((var(data) / n)) -> druhý zápis

# Dolní a horní mez intervalu spolehlivosti
lower_bound <- mean_value - error_margin
upper_bound <- mean_value + error_margin

# Výsledek
interval_spolehlivosti <- c(lower_bound, upper_bound)
interval_spolehlivosti

# Budeme testovat, zda jsou matky v prumeru mensi nez 168 cm
# testovane hypotezy: H0: vyska matek = 168 cm
#					  H1: vyska matek < 168 cm

t.test(prom1, mu=168, alternative='less') # > p-value = 0.04829 -> zamitam H0, plati H1
# prumerna vyska je menši jak 168

# t-test pracuje s testovou statistikou T: T = sqrt(n)*(mean(X) - mu)/sd(X)
# p-hodnota je pravdepodobnost, ze za platnosti nulove hypotezy
# 	nastane vysledek, ktery nastal,
# 	nebo jakykoliv jiny, ktery jeste vic vyhovuje alternative

# v pripade oboustranne alternativy pridam jeste druhou skupinu hodnot 
#Muze byt populacni prumer vysky matek 168 cm?
#   H0: vyska matek = 168 cm vs. H1: vyska matek <> 168 cm

t.test(prom1,mu=168)
# p-hodnota 0.09659 > alfa 0.05 -> nezamitame H0
#   neprokazalo se, ze by vyska matek nemohla byt rovna 168 cm

#-----------------------------------------------------------------------------
## Jednovyberovy Wilcoxonuv test
#=> neparametrický -> využívá median, není třeba normálího rozdělení

# Jsou matky v prumeru starsi nez 23 let?
prom2 <- data$vekM
# je založen na pořadích. Tento test testuje medián.

# Nejprve otestujeme normalitu
#   H0: data maji normalni rozdeleni vs. H1: data nemaji normalni rozdeleni
shapiro.test(prom2) #p-value = 0.00134 -> zamítám H0 -> nemají
PlotQQ(prom2) # -> neleží na přímce

# pouzijeme neparametricky test
# Testujeme
#   H0: median vekM = 23 vs. H1: median vekM > 23
wilcox.test(prom2,mu=23, alternative="greater")
#p-value = 9.807e-09 -> zamítáme H0 - střední hodnota je větší jak 23
MeanCI(prom2) #bude kolem 25

#-----------------------------------------------------------------------------
## Dvouvyberovy t-test
## Lisi se u porodni hmotnost mezi pohlavimi (por.hmnotnost, pohlavi)?

hmot <- data$por.hmotnost
pohlavi <- data$Hoch

# test normiality
#H0: nomrální rozdělení
#H1: není normální rozdělení

par(mfrow=c(1,2))
tapply(hmot,pohlavi,PlotQQ) # jde videt, že hoši mají jak kvantily, tak průměry vyšší
par(mfrow=c(1,1))
  #body lezi v obou pripadech priblizne na primce
tapply(hmot,pohlavi,shapiro.test)
#hmot -> 0.09265 -> nezamítáme H0
#pohlavi -> 0.6414 -> nezamítáme H0
#   => Data mají normální rozdělení

boxplot(hmot~pohlavi,main="Porodní hmotnost X Pohlaví")

# testujeme hypotezy
# H0: por.hmotnost divek - por.hmotnost hochu = 0;  H1: por.hmotnost divek - por.hmotnost hochu <> 0

# Mame na vyber dva dvouvyberove t-testy:
#   pro shodne rozptyly - t.test
#   pro ruzne rozptyly - welchův test -> t.test(hmot ~ pohlavi, var.eq = FALSE!!!!!)

# Jsou rozptyly shodne?
# H0: rozptyly se nelisi; H1: rozptyly se lisi

var.test(hmot~pohlavi)
# =>p-value = 0.886 - rozptyly jsou shodne


# H0: por.hmotnost divek - por.hmotnost hochu = 0;  H1: por.hmotnost divek - por.hmotnost hochu <> 0
t.test(hmot~pohlavi,var.eq=T)
# p-value = 0.005512 < alfa -> zamítám H0
t.test(hmot~pohlavi,mu=0,alternative='two.sided',var.eq=T) #stejné

#-----------------------------------------------------------------------------
#Dvouvyberovvy Wilcoxonuv test
# I u dvouvyberoveho Wilcoxonova testu je pozadavek na shodu rozptylu???

# Lisi se vek maminek v Praze a na venkove (vekM, Porodnice)?
vekM <- data$vekM
porodnice <- data$Porodnice

par(mfrow=c(1,2))
tapply(vekM,porodnice,PlotQQ)
par(mfrow=c(1,1))
# vekM leží na přímce, ale kraje ne
# porodnice spíše ne

# Test normality
#H0: data mají normální rozdělení | H1: nemají normální rozd.
tapply(vekM,porodnice,shapiro.test)
# vekM p-value = 0.02389 < alfa - zamítáme H0 -> nemají normální rozdělení
# porodice - p-value = 0.01879 < alfa - tamítáme H0 -> emají noérmální rozdělení

# Shoda rozptylu F-test
#H0: mají shodné rozptyly | H1: nemají shodné rozptyly
var.test(vekM~porodnice) #=> p-value = 0.6589 > alfa - nezamítáme H0, mají shodné rozptyly

boxplot(vekM~porodnice)

wilcox.test(vekM~porodnice)
# p-value = 0.09097 > alfa -> nezamítáme H0 -> v Praze a a vekově se vek matek neliší.

#-----------------------------------------------------------------------------
## ANOVA - analyza rozptylu  
library(ggbio)
library(DescTools)

force(mtcars)

# Lisi se cas, za nejz ujedou auta 1/4 mile podle poctu valcu
cislo <- mtcars$qsec
valce <- as.factor(mtcars$cyl)

#Test normality residui linearniho modelu
lm_model <- lm(cislo~valce)
pred <- predict(lm_model) # predikce hodnot

res <- residuals(lm(cislo~valce))
res

"""
A residual is the difference between a predicted value of dependent variable y with the actual value of y.
"""
res_num = cislo - pred

par(mfrow=c(1,2))
PlotQQ(res)
PlotQQ(res_num)
par(mfrow=c(1,1))

# Ciselny test - normalně shapiro
#H0: residua maji normalni rozdeleni |H1: residua NEmaji normalni rozdeleni
shapiro.test(res) #=> residua maji normali rozdeleni - p-value > alfa

boxplot(cislo~valce)
# jde vidět, že čím více válců, tim rychlejší čas.. :0 OMG!

# Testujeme hypotezy
#   H0: vsechny skupiny jsou stejne;  H1: alespon jedna skupina se lisi
#   H0: cas na poctu valcu nezavisi; H1: cas na poctu valcu zavisi

#Test shody rozptylu
# dle vysledku se voli typ ANOVy
# H0: rozptyly jsou shodne | H1: rozptyly se lisi
bartlett.test(cislo~valce) # => p-value = 0.4554 > alfa - maji shodne rozptyly
var.test(cislo~valce) #=> jenom pro dva levely kategorické proměné
"""
Error in var.test.formula(cislo ~ valce) : 
  grouping factor must have exactly 2 levels
"""

# Využití klasické anovy
anova(aov(cislo~valce))
# => p-value = 0.0003661 < alfa zamítáme h0, platí H1 - čas na počet válců závisí

# Muze prijit doplnujici otazka: ktere dvojice skupin se od sebe vyznamne lisi?
# Párové srovnání
tukey_result <- TukeyHSD(aov(cislo~valce))
plot(tukey_result)
# vidime intervaly spolehlivosti pro rozdily  největší rozdíl 8-4 válců





## Zavisi pomer os na poctu prevodu?
cislo<-mtcars$drat
kategorie<-as.factor(mtcars$gear)
res <- residuals(lm(cislo~kategorie))

# Test normality residui
#H0: mají normální rozd.| H1: emají normální roz.
PlotQQ(res) # => spíše nemají
shapiro.test(res)
# => p-value = 0.001428 < alfa -> zamítám H0 -> residua nemají normální rozdělení
#=> proto použiji Kruskal-Wallisův test

boxplot(cislo~kategorie) #=> hodne se liší auta s 3 převody

# Test shody rozptylu - i neparametricky test ocekava priblizne stejne rozptyly!
#H0: rozptyly se neliší | H1: rozptyly se liší
bartlett.test(cislo~kategorie)
# => p-value = 0.6596 > alfa -> nezamítám H0; rozptyly jsou přibližně shodné

#Hypotézy
#H0: NEzávisí pomer os a pocet převodu | H1: Závisí pomer os a pocet prevodu

kruskal.test(cislo~kategorie)
# => p-value = 2.242e-05 < alfa --> zamítáme H0, Poměr os závisí s poctem převodů závisí

# Ktera dvojice skupin se od sebe vyznamne lisi?
DunnTest(cislo~kategorie)
# Nejvíce se liší vozy se třemy převody


# Analyza rozptylu pro pripad, ze se lisi variabilita ve skupinach
oneway.test(cislo~kategorie, var.equal = FALSE)



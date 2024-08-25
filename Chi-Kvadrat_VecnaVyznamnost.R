library(DescTools)
library(TeachingDemos)
data(mtcars)

### Chi-kvadrat test
# pro 2 a více kategorické proměnné -> chi kvadrát

## Souvisi spolu pocet valcu a typ prevodovky?
cyl <- mtcars$cyl
trans <- mtcars$am

mtcars

levels(cyl)
levels(trans)


plot(as.factor(cyl)~as.factor(trans))

#Hypotézy - nezávislost:
# H0: pocet valcu a typ prevodovky spolu nesouvisi
# H1: pocet valcu a typ prevodovky spolu souvisi

chisq.test(cyl,trans)
#-> p-value = 0.01265 < alfa -> zamítám H0
#-> spolu souvisí

"""
Warning message:
In chisq.test(cyl, trans) : 
Chi-squared approximation may be incorrect
===> nejsouu splěny předpoklady
"""
chisq.test(cyl,trans)$ex
# jedna ocekavana cetnost je mensi nez 5
# => druhý sloupec!

#===> když to failne, tak musíme na exaktni
#   test - Fisheruv

#Fisheruv test
# => faktoriálový test -> vytvoří tabulku s pst pro jednotlivé varianty
fisher.test(cyl,trans)
# =- p-value = 0.009105 < alfa - zamítám H0
# Pocet valcu a typ prevodovky spolu souvisi




## Souvisi spolu typ motoru a typ prevodovky (promenne vs a am)
motor <- mtcars$vs
trans #převodovka

plot(as.factor(motor), as.factor(trans))

#Hypotézy:
# H0: Motor a typ převodovky spolu NEsouvisí
# H1: Motor a typ převodovky spolu souvisí

chisq.test(motor,trans)
# => p-value = 0.5555 > alfa - > nezámtíme H0
chisq.test(motor,trans)$ex #=> četnost je ok!

# Jiny vystup pro Fisheruv exaktni test
fisher.test(motor,trans)
# => p-value = 0.4727 > alfa - > nezámtíme H0


################################
### Nacteni dat Stulong.RData
## data z velke studie, ktera u muzu stredniho veku merila riziko srdecni choroby

names(Stulong)<-c("ID","vyska","vaha","syst1","syst2","chlst","vino","cukr",
                  "bmi","vek","KOURrisk","Skupina","VekK")

###################
### Vecna vyznamnost
library(effectsize)
library(DescTools)
  
## Je vyznamny rozdil ve vysce mezi starsimi a mladsimi muzi? (promenne vyska, VekK)
ciselna<-Stulong$vyska
kategoricka<-Stulong$VekK

#Test normality
par(mfrow=c(1,2))
tapply(ciselna,kategoricka,PlotQQ)
par(mfrow=c(1,1))

# Shapiro
# H0: je normalni | H1: NENI normalni
tapply(ciselna,kategoricka,shapiro.test)
# zamítáme H0 - ani jedna nema norm. rozd.

#F-test - var.test() - test shody rozptylu
var.test(ciselna~kategoricka)
# p-value = 0.1763 > alfa -> nezamítáme H0 - rozptyly se neliší!


# Podle sheetu ze cviceni pouzij t.test i wilcoxe..

t.test(ciselna~kategoricka)
wilcox.test(ciselna~kategoricka)
# ==> zamítáme podle obou testů -> není skutečně významný



### Statistiky vecne vyznamnosti
# věcná významnost odpovídá na otázku, zda je výsledek natolik velký,
# aby měl reálný, praktický význam.


"""
Věcná významnost se zaměřuje na to, zda je nalezený efekt natolik významný, aby měl praktickou hodnotu, zatímco statistická 
významnost se soustředí na pravděpodobnost, že je nalezený efekt reálný a ne jen výsledkem náhody. 

Korelační koeficient je nástroj, 
který může být použit k hodnocení vztahů mezi proměnnými, 
  a tím i k posouzení věcné významnosti těchto vztahů.

"""

### Pro dvouvýběrový
cohens_d(ciselna~kategoricka)
# Cohenovo d
interpret_cohens_d(cohens_d(ciselna~kategoricka))

# Hedgesovo g
hedges_g(ciselna~kategoricka)
interpret_hedges_g(hedges_g(ciselna~kategoricka))

# Glassovo delta
glass_delta(ciselna~kategoricka)
interpret_glass_delta(glass_delta(ciselna~kategoricka))



### Pro ANOVA - ciselna x kategorická[>2]
eta_squared(aov(ciselna~kategoricka))
# Fisherovo eta
(A<-anova(aov(ciselna~kategoricka)))
A[,2]
A[1,2]/(sum(A[,2]))

omega_squared(aov(ciselna~kategoricka))
# Haysova omega
(A[1,2]-A[2,3])/(sum(A[,2])+A[2,3])

epsilon_squared(aov(ciselna~kategoricka))

#-------------------------------------------------------------

## Souvisi spolu diagnosticka Skupina a vek muzu (promenne Skupina, VekK)
kat1<-Stulong$Skupina
kat2<-Stulong$VekK

#Hypotézy:
#H0: nesouvisí | H1: souvisí

# Teď pomocí výnzmanosti řešíme JAK MOC spolu soluviší - jak velký je tenefekt..
tab <- table(kat1,kat2)
plot(as.factor(kat1),as.factor(kat2))
chisq.test(kat1,kat2)
# => p-value = 0.001267 < alfa -> zamítáme H0 -> spolu souvisí


chisq_to_cramers_v(chisq.test(tab)$statistic,
                   n = sum(tab),
                   nrow = nrow(tab),
                   ncol = ncol(tab)
)


# Cramerovo V - numericky
sqrt(chisq.test(tab)$statistic/(sum(tab)*(ncol(tab)-1)))





## Souvisi spolu konzumace vina a vek muzu (promenne vino, VekK)
vino<-Stulong$vino
vekK<-Stulong$VekK

# Cramer V s tabulkou
tab2 <- table(vino,vekK)
sqrt(chisq.test(tab2)$statistic/(sum(tab2)*(ncol(tab2)-1)))

#Hypotézy:
#H0: nesouvisí | H1: souvisí

chisq.test(vino,vekK)
# => p-value = 0.08323 -> nesouvisí, nezamítáme H0

# Cramer V - numericky bez tabulky
cramer_v <- sqrt(chisq.test(vino,vekK)$statistic/(length(vino)*(length(levels(vekK))-1)))
cramer_v

# Funkce
chisq_to_cramers_v(chisq.test(tab2)$statistic,
                   n = sum(tab2),
                   nrow = nrow(tab2),
                   ncol = ncol(tab2)
)



## Souvisi spolu vaha a hladina cholesterolu?
# dvě spojité proměnné (číselné), tak použiji korelační koeficient
cislo1<- Stulong$vaha
cislo2<-Stulong$chlst

plot(cislo1~cislo2,pch=19,main="Souvislost vahy a hladiny cholesterolu")

cor(cislo1,cislo2) #čistě korelačí koef
cor.test(cislo1,cislo2) # jak koef tak i p-value
# Zavislost je statisticky vyznamna
# H0: neí závilost |H1: je závislost
interpret_r(cor(cislo1,cislo2)) # hodí i kategorii - small medium...
# => závislost tam je, ale velice ízká - 0.11, což je velice nízká

summary(lm(cislo1~cislo2))$r.squared  
# koeficient determinace
# kolik procent variability zavisle promenne se modelem vysvetlilo
interpret_r2(summary(lm(cislo1~cislo2))$r.squared)


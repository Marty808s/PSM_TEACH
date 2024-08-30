# Otevrete datovy soubor Kojeni2.RData
#   data pouzita k vypracovani diplomove prace na PRF UK v roce 2000

library(DescTools)
library(TeachingDemos)
# aktivace knihoven

#################################
## Jednovyberovy t-test

# Na zaklade intervalu spolehlivosti rozhodnete, zda stredni hodnota
#   vysky matek muze byt 164 cm?
#   A co 165 cm, 166 cm, 167 cm, 168 cm,  169 cm, ... ?
prom1<-Kojeni2$vyskaM
MeanCI(prom1)
  # rozhodnuti by melo byt stejne, jako u jednovyberoveho t-testu
  t.test(prom1,mu=164)

## Vykresleni p-hodnoty
# Jednovyberovy test, nejprve pro jednostrannou alternativu 
# Budeme testovat, zda jsou matky v prumeru mensi nez 168 cm
# testovane hypotezy: H0: vyska matek = 168 cm
#					  H1: vyska matek < 168 cm
# pozor: test proti jednostranne alternative ma jednostranny interval spolehlivosti
t.test(prom1,mu=168,alternative="less")
  # p-hodnota = 0.04829 < alfa (= 0.05) -> zamitam H0, plati H1
  # Stredni hodnota vysky matek je mensi nez 168 cm.

# t-test pracuje s testovou statistikou T: T = sqrt(n)*(mean(X) - mu)/sd(X)
#	pro tuto statistiku je pak definovana p-hodnota

# p-hodnota je pravdepodobnost, ze za platnosti nulove hypotezy
# 	nastane vysledek, ktery nastal,
# 	nebo jakykoliv jiny, ktery jeste vic vyhovuje alternative

# graf - definice p-hodnoty
# 1. pravdepodobnost, ze za platnosti nulove hypotezy ...
#   H0 rika, ze testova statistika a ma t-rozdeleni s n-1 stupni volnosti
plot(x<-seq(-4,4,by=0.1),y=dt(x,length(prom1)-1),type="l",col="blue",main="Hustota t-rozdeleni za H0")
  # hustota t-rozdeleni
# 2. nastane vysledek, ktery nastal ...
(T<-sqrt(length(prom1))*(mean(prom1)-168)/sd(prom1))
  # testova statistika 
lines(c(T,T),c(0,dt(T,length(prom1)-1)),col="red",lwd=2)
  # zakreslim do grafu
# 3. nebo jakakoliv jina hodnota, ktera jeste vic odpovida alternative
#   alternativa je mensi nez
xx<-c(seq(-4,T,by=0.1),T,T,-4)
yy<-c(dt(c(seq(-4,T,by=0.1),T),length(prom1)-1),0,0)
polygon(xx,yy,density=40,col="red")

# rucni vypocet p-hodnoty
# pravdepodobnost hodnot mensich nez testova statistika T
pt(T,length(prom1)-1)
  # p-hodnota je distribucni funkce v hodnote testove statistiky

# v pripade oboustranne alternativy pridam jeste druhou skupinu hodnot 
#   - symetricky podle testove statistiky T
#	U otazky typu: Muze byt populacni prumer vysky matek 168 cm?
#   H0: vyska matek = 168 cm vs. H1: vyska matek <> 168 cm
xx2<-c(-T,-T,seq(-T,4,by=0.1),4,-T)
yy2<-c(0,dt(c(-T,seq(-T,4,by=0.1)),length(prom1)-1),0,0)
polygon(xx2,yy2,density=40,col="green")

# rucni vypocet p-hodnoty
pt(T,length(prom1)-1)+1-pt(-T,length(prom1)-1)
  2*pt(T,length(prom1)-1)
# pst je symetricka kolem nuly
t.test(prom1,mu=168)
  # kontrolni test
  # p-hodnota 0.09659 > alfa 0.05 -> nezamitame H0
  #   neprokazalo se, ze by vyska matek nemohla byt rovna 168 cm
  #   muze byt rovna 168 cm, nebo je priblizne rozvna 168 cm.

# Jake rozdeleni ma p-hodnota za platnosti nulove hypotezy? 
#	Jaka je pst, ze Vam vyjde p < 0.05? A jaka je pst, ze Vam vyjde p < 0.5?
# Vyzkousime empiricky
#	Predpokladejme, ze IQ ma normalni rozdeleni se stredni hodnotou 100 a rozptylem 225
#   provedeme nahodny vyber z tohoto rozdeleni o rozsahu 200
#	  a otestujeme nulovou hypotezu, ze stredni hodnota = 100
#   ziskanou p-hodnotu zakreslim do grafu a cely postup opakuji 1000 krat
#	vysledkem bude graf rozdeleni p-hodnot

N <- 1000			# pocet vyberu
n <- 200			# pocet pozorovani v jednom vyberu
p.hodnoty <- rep(0,N)	# prazdny vektor pro prumery
for (i in 1:N){
  vyber<-round(rnorm(n,100,sqrt(225)),0)
  p.hodnoty[i] <- t.test(vyber,mu=100)$p.value
}
hist(p.hodnoty)
  # v idealnim pripade by vysly vsechny sloupce stejne vysoke
(Y = sum(p.hodnoty<=0.05))
  # v kolika pripadech vysla p-hodnota < 0.05
(Y = sum(p.hodnoty<=0.5))
  # v kolika pripadech vysla p-hodnota < 0.5

# => p-hodnota ma rovnomerne rozdeleni na intervalu [0,1]

## Co je sila testu
# Pravdepodobnost, ze zamitnu nulovou hypotezu, kdyz plati vybrana alternativa
power.examp()
power.examp(diff=3)
power.examp(n=25)
power.examp(alpha=0.1)

###################################

## Zjistete, zda stredni hodnota porodni hmotnosti deti muze byt 3.5 kg.
#   POZOR: porodni hmotnost se meri v gramech!
prom2<-Kojeni2$por.hmotnost

# predpokladem jednovyberoveho t-testu je normalita dat
# Nejprve otestujeme normalitu
#   H0: data maji normalni rozdeleni vs. H1: data nemaji normalni rozdeleni
PlotQQ(prom2)
  # body lezi priblizne na primce
shapiro.test(prom2)
  # p-hodnota 0.1623 > alfa => nezamitam H0
  # i Q-Q plot, i test normality ukazuji, ze promenna ma priblizne normalni rozdeleni

# pouziti jednovyberoveho t-testu je v poradku
# Testovane hypotezy: H0: porodni hmotnost = 3500 g  vs. H1: porodni hmotnost <> 3500 g
t.test(prom2,mu=3500)
  # p-hodnota = 0.4943 > alfa => nezamitam H0
  # stredni porodni hmotnost deti muze byt 3.5 kg.

# Muze byt stredni hodnota vysky otcu vetsi nez 177 cm? (promenna vyskaO)
# Muze byt hmotnost pulrocni deti v prumeru 7.5 kg? (promenna hmotnost)

##################
## Jednovyberovy Wilcoxonuv test

# Jsou matky v prumeru starsi nez 23 let?
prom3<-Kojeni2$vekM

# Nejprve otestujeme normalitu
#   H0: data maji normalni rozdeleni vs. H1: data nemaji normalni rozdeleni
PlotQQ(prom3)
  # body lezi na oblouku - mam sesikmene rozdeleni
shapiro.test(prom3)
  # p-hodnota 0.00134 < alfa => zamitam H0
  # i Q-Q plot, i test normality ukazuji, ze promenna nema normalni rozdeleni

# pouzijeme neparametricky test
# Testujeme
#   H0: median vekM = 23 vs. H1: median vekM > 23

# Wilcoxonuv test 
wilcox.test(prom3,mu=23,alternative="greater")
  # p-hodnota 9.807e-09 < alfa 0.05 -> zamitam H0
  # Prokazali jsme, ze stredni hodnota veku matek je vetsi nez 23 let.

# Muze byt stredni hodnota delky pulrocnich deti 72 cm? (promenna delka)

##############################
## Dvouvyberovy t-test

## Lisi se u porodni hmotnost mezi pohlavimi (por.hmnotnost, pohlavi)?
cislo<-Kojeni2$por.hmotnost
kategorie<-Kojeni2$Hoch

# Normalita se testuje pro kazdou skupinu zvlast
#   H0: data maji normalni rozdeleni vs. H1: data nemaji normalni rozdeleni
par(mfrow=c(1,2))
tapply(cislo,kategorie,PlotQQ)
par(mfrow=c(1,1))
  # body lezi v obou pripadech priblizne na primce
tapply(cislo,kategorie,shapiro.test)
  # obe p-hodnoty vetsi nez alfa -> nezamitame H0, 
  #   data maji normalni rozdeleni -> pouziji t-test

# nejprve graficke zobrazeni
boxplot(cislo~kategorie,main="Porodni hmotnost podle pohlavi",col=c(2,4))
  # vidime poradi kategorii, muze se hodit

# testujeme hypotezy
# H0: por.hmotnost divek - por.hmotnost hochu = 0;  H1: por.hmotnost divek - por.hmotnost hochu <> 0

# Mame na vyber dva dvouvyberove t-testy:
#   pro shodne rozptyly
#   pro ruzne rozptyly

# Jsou rozptyly shodne?
# H0: rozptyly se nelisi; H1: rozptyly se lisi
var.test(cislo~kategorie)
  # p-hodnota = 0.886 > alfa (0.05) -> nezamitame H0
  #   rozptyly jsou priblizne shodne, pouzijeme t-test pro shodne rozptyly
t.test(cislo~kategorie,var.eq=T)
  t.test(cislo~kategorie,mu=0,alternative="two.sided",var.eq=T)
  # p-hodnota = 0.005512 < alfa (0.05) -> zamitame H0, plati H1
  #   porodni hmotnost se lisi podle pohlavi
  # A kdo ji ma vyssi?

# Jsou pulrocni kluci v prumeru tezsi nez pulrocni divky? (promenne hmotnost, Hoch)
# Jsou matky v Praze i na venkove stejne vysoke? (promenne vyskaM, Porodnice)

###############################
## Dvouvyberovy Wilcoxonuv test

## Lisi se vek maminek v Praze a na venkove (vekM, Porodnice)?
cislo<-Kojeni2$vekM
kategorie<-Kojeni2$Porodnice

# Normalita se testuje pro kazdou skupinu zvlast
#   H0: data maji normalni rozdeleni vs. H1: data nemaji normalni rozdeleni
par(mfrow=c(1,2))
tapply(cislo,kategorie,PlotQQ)
par(mfrow=c(1,1))
  # body lezi na obloucich v obou pripadech
tapply(cislo,kategorie,shapiro.test)
  # obe p-hodnoty < alfa -> zamitame H0, data nemaji normalni rozdeleni
  
# Wilcoxonuv test
#   H0: matky z Prahy - matky z venkova = 0;  H1: matky z Prahy - matky z venkova <> 0
# Graficky
boxplot(cislo~kategorie, main="Vek matky podle mista porodu")
  # vidime poradi kategorii, muze se hodit
  
# I u dvouvyberoveho Wilcoxonova testu je pozadavek na shodu rozptylu
#   H0: rozptyly se nelisi; H1: rozptyly se lisi
var.test(cislo~kategorie)
  # p-hodnota = 0.6589 > alfa (0.05) -> nezamitame H0
  # predpoklad shody rozptylu je splnen
wilcox.test(cislo~kategorie)
  # p-hodnota = 0.09097 > alfa (0.05) -> nezamitame H0
  #   neprokazalo se, ze by se vek matek v Praze a na venkove vyznamne lisil.
  
## Jsou matky, ktere daly detem dudlika, v prumeru starsi nez ty, co jim ho nedaly? (promenne vekM, Dudlik)
## Je rozdil ve veku matek, ktere jeste v pul roce kojily a tech co nekojily? (promenne vekM, Koj24)


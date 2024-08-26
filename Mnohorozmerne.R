#######################################
#### Mnohorozmerna statistika
# => nemáme jednu prům / střední hodnotu, ale máme vektor středních hodnot
#   ===> měříme váhu,věk, tlak, vit. kapacita plic...
#   ===> různé známky na ZŠ
#=> máme vektor středíních hodnot - (X1,X2,X3,X4...Xn)

# Základem je měření vzdáleosti mezi dvěma body => Eukleidovská / Mahalanobisova

# Zobecnění - dvouvýběrovvý ==> Hotellingův test
# ANOVA => MANOVA
# Korelačí koef => Kanonické korelace
# Lin. reg. => Mnohonásobá lin. reg.

library(DescTools)

# Porovnavame 2 vyucujici na zaklade hodnoceni jejich studentu. 
#   Je mezi vyucujicimi vyznamny rozdil?

matematici <- data.frame(ucitel = factor(rep(1:2, c(5, 7))), 
                         spokojenost = c(1, 3, 2, 4, 3, 2, 6, 4, 5, 5, 3, 4), 
                         znalost = c(4, 7, 2, 6, 3, 6, 6, 8, 7, 10, 9, 6))
matematici


tapply(matematici$spokojenost,matematici$ucitel,mean)
tapply(matematici$znalost,matematici$ucitel,mean)


# Jednorozměrné porovnání - zde porovnavame kazdou promennou zvlast
#H0: NEliší zavislost - proměnna X ucitel | H1: liší - proměnna X ucitel
t.test(matematici$spokojenost~matematici$ucitel)
# p-value = 0.05893 - nezamitame H0
t.test(matematici$znalost~matematici$ucitel)
# p-value = 0.02828 < alfa -> tady se liší

boxplot(matematici$spokojenost~matematici$ucitel)
boxplot(matematici$znalost~matematici$ucitel)

# Vícerozměrně - Hotellingův test
m1 <- HotellingsT2Test(cbind(matematici$spokojenost, matematici$znalost) ~ matematici$ucitel)
m1

# df1 = 2, df2 = 9, p-value = 0.04188 < alfa - takže se liší
# ==> to df2 ukazuje na znalost -> stejně jako t-test

#T2 rozdělení -> Testovací statistika: Hotellingovo 
#===> je statistika, která je vypočítána na základě rozdílu mezi vektory průměrů a korelační matice proměnných.




###MANOVA
## MANOVA - jak se vytvari plasticky film
# vytvoreni dat
trhliny <- c(6.5, 6.2, 5.8, 6.5, 6.5, 6.9, 7.2, 6.9, 6.1, 6.3, 6.7, 6.6, 7.2, 7.1, 6.8, 7.1, 7.0, 7.2, 7.5, 7.6)
lesk <- c(9.5, 9.9, 9.6, 9.6, 9.2, 9.1, 10.0, 9.9, 9.5, 9.4, 9.1, 9.3, 8.3, 8.4, 8.5, 9.2, 8.8, 9.7, 10.1, 9.2)
sytost <- c(4.4, 6.4, 3.0, 4.1, 0.8, 5.7, 2.0, 3.9, 1.9, 5.7, 2.8, 4.1, 3.8, 1.6, 3.4, 8.4, 5.2, 6.9, 2.7, 1.9)

data <- cbind(trhliny,lesk,sytost)
data

# Vytvoření kategorií
pomer <- factor(gl(2,10), labels=c("Nizky", "Vysoky"))
prisady <- factor(gl(2, 5, length=20), labels=c("Nizky", "Vysoky"))


fit <- manova(data ~ pomer * prisady)


## TO:DO - Dojet a pochopit jednotlivé metody - Pillai.... - B;W..
# Vypíšuju jednotlivé analýzy rozptylu pro každou proměnnou
summary.aov(fit)

# R-ko nabizi statistiky: "Pillai" --> default, "Wilks", "Hotelling-Lawley", "Roy"
summary(fit, test="Wilks")

summary(fit)
summary(fit, test="Hotelling-Lawley")


###PCA - metoda hlavnich komponent
v1 <- c(1,1,1,1,1,1,1,1,1,1,3,3,3,3,3,4,5,6)
v2 <- c(1,2,1,1,1,1,2,1,2,1,3,4,3,3,3,4,6,5)
v3 <- c(3,3,3,3,3,1,1,1,1,1,1,1,1,1,1,5,4,6)
v4 <- c(3,3,4,3,3,1,1,2,1,1,1,1,2,1,1,5,6,4)
v5 <- c(1,1,1,1,1,3,3,3,3,3,1,1,1,1,1,6,4,5)
v6 <- c(1,1,1,2,1,3,3,3,4,3,1,1,1,2,1,6,5,4)

m1 <- cbind(v1,v2,v3,v4,v5,v6)

# Zakladem analyzy hlavnich komponent je korelacni matice
# ==> zaklad je u korelace...

cor(m1) # <- korelační matice
eigen(cor(m1)) #TO:DO - eigen co to je!?

# Vizualizace hlavnich komponent - elbow point
# x-> komponenty; y-> představují množství vysvětlené variance každou komponentou. 
screeplot(princomp(m1),type="l")
abline(h=1,col="green")

#Vypis variability daných komponent
cumsum(eigen(cor(m1))$values/sum(eigen(cor(m1))$values))
# prvni 3 komponenty vysvetli pres 90% variability

# Analýza hlavnich komponenty
# ==> eigen vector
prcomp(m1)
(PC<-prcomp(m1))
# vrati variabilitu hlavnich komponent spolu s koeficienty jednotlivych komponent
plot(PC)



## Faktorova analyza

factanal(m1, factors = 3)
# faktorova analyza jen prerotuje hlavni komponenty
#	metoda rotace 'varimax' je brana jako zakladni (defaultni) 
# vypis loadingu a procent vysvetlene variability
(sc<-factanal(~v1+v2+v3+v4+v5+v6, factors = 3,scores = "Bartlett")$scores)
sc
plot(sc[,1],sc[,2],pch=19, main="Prvni 2 faktory")
# vykresleni prvnich dvou faktoru
################################
## Samostatne
# Nacteni dat Ichs.RData

# Zavisi biologicke ukazatele (vaha, vyska a oba tlaky) na vzdelani a koureni?
#   promenne vyska, hmot, syst, diast, Vzdel a Koureni

# Urcete hlavni komponenty a faktory pro promenne vyska, hmot, syst, diast a vek

# V datovem souboru mtcars sledujte zavislost vykonostnich charakteristik
#   (mpg, disp, hp, drat, wt, qsec) na ostatnich promennych

# Kolik hlavnich komponent/ faktoru je potreba pro reprezentaci vykonostnich promennych?
#   A jake to jsou?

################################
# Zavisi biologicke ukazatele (vaha, vyska a oba tlaky) na vzdelani a koureni?
#   promenne vyska, hmot, syst, diast, Vzdel a Koureni

data <- Ichs

vyska <- data$vyska
hmotnost <- data$hmot
syst <- data$syst
dias <- data$diast

vzdelani <- factor(data$Vzdel)
koureni <- factor(data$Koureni)

dataaov <- cbind(vyska, hmotnost, syst, dias)

model <- manova(dataaov ~ vzdelani * koureni)
summary.aov(model)

# Z analýzy rozptylu vidíme, že tlak (syst a dias) je ovlivněn kouřením
# P values: 3.364e-05 *** | syst, 0.0001267 *** | dias
# Dále výška je ovlivněna vzděláním (pravdpěodobně ZŠ/SŠ/VŠ -> věkový rozdíl)
# P value: 0.02367 *
################################
# Urcete hlavni komponenty a faktory pro promenne vyska, hmot, syst, diast a vek
vyska <- data$vyska
hmotnost <- data$hmot
syst <- data$syst
dias <- data$diast
vek <- data$vek


data2 <- cbind(vyska, hmotnost, syst, dias, vek)
cor(data2)
eigen(cor(data2))

screeplot(princomp(data2),type="l")
abline(h=1,col="green")

prcomp(data2)
cumsum(eigen(cor(data2))$values/sum(eigen(cor(data2))$values))
# [1] 0.4064445 0.6748415 0.8675851 0.9598314 1.0000000
factanal(data2, factors=2)
# Hlavní komponenty jsou tlak (syst + dias) a tělesná stavba (hmotnost + vyska)
#         Factor1 Factor2
#vyska             0.667 
#hmotnost  0.198   0.761 
#syst      0.981   0.121 
#dias      0.797   0.121 
#vek              -0.135 
#Factor1 Factor2
#SS loadings      1.641   1.072
#Proportion Var   0.328   0.214
#Cumulative Var   0.328   0.543

# Faktorová analýza nám vysvětluje 54.3% variability (není úplně dobrý model? - málo faktorů)
################################
# V datovem souboru mtcars sledujte zavislost vykonostnich charakteristik
#   (mpg, disp, hp, drat, wt, qsec) na ostatnich promennych
cars <- mtcars

mpg <- cars$mpg
disp <- cars$disp
hp <- cars$hp
drat <- cars$drat
wt <- cars$wt
qsec <- cars$qsec

cars2 <- cbind(mpg, disp, hp, drat, wt, qsec)
cars3 <- cbind(cars$cyl, cars$vs, cars$am, cars$gear, cars$carb)

model <- manova(cars2 ~ cars$cyl * cars$vs * cars$am * cars$gear * cars$carb)

summary.aov(model)
# Závislost výkonostních charakteristik je značná hlavně u proměnných cyl, vs, am
# P values: 1.648e-06 *** | cyl, 4.477e-05 *** | vs, 3.252e-05 *** | am
################################
# Kolik hlavnich komponent/ faktoru je potreba pro reprezentaci vykonostnich promennych?
#   A jake to jsou?
cor(cars2)
eigen(cor(cars2))


screeplot(princomp(cars2),type="l")
abline(h=1,col="green")

prcomp(cars2)
cumsum(eigen(cor(cars2))$values/sum(eigen(cor(cars2))$values))
# [1] 0.6978994 0.8892514 0.9448109 0.9705376 0.9913370 1.0000000
factanal(cars2, factors=2)
# Pro reprezentaci výkonnostních proměnných jsme určili tyto 2 faktory:
# Výkonnost (disp + hp + wt)
# Rychlost (qsec + mpg)
################################

## Kanonicka korelace
#	korelace mezi dvema skupinami promennych
# pracujme s charakteristikami statu: osobni uspory, podil populace do 15 let,
#	podil populace nad 75 let, prijem na obyvatele, narust prijmu na obyvatele
# rozdelime promenne do skupin: populacni podily, ekonomicke charakteristiky
pop <- LifeCycleSavings[, 2:3]
oec <- LifeCycleSavings[, -(2:3)]
cancor(pop, oec)
# TODO
# vypocet kanonickych korelaci
#	na vystupu jsou kanonicke korelace (jejich pocet je stejny jako 
#	pocet promennych v mensi skupine), koeficienty kanonickych promennych
#	prumery promennych


################################
## Diskriminační analýza
library(MASS)
# databaze o trech druzich kosatcu: Setosa (s), Versicolour (c), Virginica (v)
# mereny jsou 4 ukazatele: sepal length & width, petal length & width
#	kalisni a okvetni listek, vzdy delka a sirka
Iris <- data.frame(rbind(iris3[,,1], iris3[,,2], iris3[,,3]),Sp = rep(c("s","c","v"), rep(50,3)))

# nahodny vyber 75 rostlin z cele databaze
train <- sample(1:150, 75) # 75 náhodných čísel v intervalu 1-150
table(Iris$Sp[train])
#tvorba trénovacích dat

# vstupni data do diskriminacni analyzy ... rostliny, u nichz presne zname druh
(z <- lda(Sp ~ ., Iris, prior = c(1,1,1)/3, subset = train))
# linearni diskriminacni analyza
# vystup: vstupni (apriori) PRAVDĚPODOBNOSTI ... jake je ocekavane zastoupeni skupin v populaci 
#	prumery promennych ve skupinach a koeficienty linearnich diskriminacnich funkci

predpovedi<-predict(z, Iris[-train, ])
predpovedi$x
# vysledne hodnoty diskriminacnich funkci
predpovedi$posterior
# pravdepodobnosti zarazeni do jednotlivych populaci
predpovedi$class
# na zaklade vytvorene klasifikacni funkce priradi nova mereni do skupin
#	vybere idealni skupinu + vypocte pravdepodobnosti s nimiz do jednotlivych skupin patri
table(Iris[-train,"Sp"],predpovedi$class)
# klasifikacni tabulka, jak dobre se trefil: v radcich skutecne hodnoty, ve sloupcich predikce
plot(predpovedi$x[,1],predpovedi$x[,2],pch=19,col=predpovedi$class,
     main="Graf diskriminacnich funkci",xlab="LD1",ylab="LD2")
legend(9,2.2,legend=c(unique(predpovedi$class)),pch=19,col=1:3)
# graf ukazujici kvalitu klasifikace
################################
## Shlukova analyza
# budem delit americke staty do skupin na zaklade 4 ukazatelu: vrazdy, napadeni, populace, znasilneni
# hierarchicke clusterovani
hc <- hclust(dist(USArrests), "ave")
# average linkage
hc <- hclust(dist(USArrests))
# complete linkage
# vstupem je matice vzdalenosti jednotlivych bodu
plot(hc, hang = -1)
# nakresleni dendrogramu - postup, jak shlukuje
#	nejprve ma kazde pozorovani svou vlastni skupinu, a ty se pak spojuji do vetsich celku
#	mozny je i obraceny postup, tj. od jedne velke skupiny k mnoha malym
seg<-cutree(hc,k=4)
# rozdeli data do 4 skupin
table(seg)
rect.hclust(hc, k=4, border="red")
# zobrazi skupiny do grafu
tapply(USArrests$Murder,as.factor(seg),mean)
# spocita prumery za jednotlive shluky

# K-means clustering
require(graphics)
# prace s dvojrozmernymi daty
x <- rbind(matrix(rnorm(100, sd = 0.3), ncol = 2),matrix(rnorm(100, mean = 1, sd = 0.3), ncol = 2))
colnames(x) <- c("x", "y")
# vygenerovani dat
(cl <- kmeans(x, 2))
# rozdeleni dat do dvou segmentu (pocet segmentu vybiram podle 'potreby')
# mohu zkusit vice seskupeni a porovnat je mezi sebou
plot(x, col = cl$cluster,pch=19)
# zakresleni dat rozdelenych do skupin
points(cl$centers, col = 1:2, pch = 8, cex=2,lwd=2)
# zakresleni stredu
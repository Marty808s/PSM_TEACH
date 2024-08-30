#######################################
#### Mnohorozmerna statistika

## Mnohorozmerny dvouvyberovy T-test (Hotellingovo T2)
library(DescTools)
# nacteni knihovny
# Porovnavame 2 vyucujici na zaklade hodnoceni jejich studentu. 
#   Je mezi vyucujicimi vyznamny rozdil?
matematici <- data.frame(ucitel = factor(rep(1:2, c(5, 7))), 
                         spokojenost = c(1, 3, 2, 4, 3, 2, 6, 4, 5, 5, 3, 4), 
                         znalost = c(4, 7, 2, 6, 3, 6, 6, 8, 7, 10, 9, 6))
  # vytvoreni dat
matematici
  # ukazka dat
tapply(matematici$spokojenost,matematici$ucitel,mean)
tapply(matematici$znalost,matematici$ucitel,mean)
  # prumerna spokojenost a znalost u jednotlivych ucitelu
# jednorozmerne porovnani
boxplot(matematici$spokojenost~matematici$ucitel)
boxplot(matematici$znalost~matematici$ucitel)
t.test(matematici$spokojenost~matematici$ucitel)
t.test(matematici$znalost~matematici$ucitel)
  # u znalosti vychazi vyznamny rozdil, u spokojenosti ne
(m1 <- HotellingsT2Test(cbind(matematici$spokojenost, matematici$znalost) ~ matematici$ucitel))
  # porovnani obou hodnoceni u ucitelu

## MANOVA - jak se vytvari plasticky film
# vytvoreni dat
trhliny <- c(6.5, 6.2, 5.8, 6.5, 6.5, 6.9, 7.2, 6.9, 6.1, 6.3, 6.7, 6.6, 7.2, 7.1, 6.8, 7.1, 7.0, 7.2, 7.5, 7.6)
lesk <- c(9.5, 9.9, 9.6, 9.6, 9.2, 9.1, 10.0, 9.9, 9.5, 9.4, 9.1, 9.3, 8.3, 8.4, 8.5, 9.2, 8.8, 9.7, 10.1, 9.2)
sytost <- c(4.4, 6.4, 3.0, 4.1, 0.8, 5.7, 2.0, 3.9, 1.9, 5.7, 2.8, 4.1, 3.8, 1.6, 3.4, 8.4, 5.2, 6.9, 2.7, 1.9)
Y <- cbind(trhliny, lesk, sytost)
  # zavisle promenna se sklada ze tri dilcich promennych
pomer <- factor(gl(2,10), labels=c("Nizky", "Vysoky"))
prisady <- factor(gl(2, 5, length=20), labels=c("Nizky", "Vysoky"))
  # dva nezavisle faktory ... tri zavisle promenne se budou porovnavat v techto skupinach

(fit <- manova(Y ~ pomer * prisady))
  # vlastni model - na vystupu jsou soucty ctvercu pro kazdou promennou
summary.aov(fit)
  # tabulky jednorozmernych analyz rozptylu pro kazdou promennou zvlast
  # na nezavisle promennych zavisi jen trhliny a lesk
summary(fit, test="Wilks")
  # existuje nekolik testovych statistik na nichz je zalozena mnohorozmerna analyza rozptylu
  # R-ko nabizi statistiky: "Pillai", "Wilks", "Hotelling-Lawley", "Roy"
  # Wilkovo lambda je zobecnenim klasicke F-statistiky z jednorozmerne ANOVy
summary(fit)
  summary(fit, test="Hotelling-Lawley")
  # pouziti jine testove statistiky
  # interakce nejsou vyznamne
(fit2 <- manova(Y ~ pomer + prisady))
summary(fit2)
  # mira vlivu samostatnych promennych

# Metoda hlavnich komponent
v1 <- c(1,1,1,1,1,1,1,1,1,1,3,3,3,3,3,4,5,6)
v2 <- c(1,2,1,1,1,1,2,1,2,1,3,4,3,3,3,4,6,5)
v3 <- c(3,3,3,3,3,1,1,1,1,1,1,1,1,1,1,5,4,6)
v4 <- c(3,3,4,3,3,1,1,2,1,1,1,1,2,1,1,5,6,4)
v5 <- c(1,1,1,1,1,3,3,3,3,3,1,1,1,1,1,6,4,5)
v6 <- c(1,1,1,2,1,3,3,3,4,3,1,1,1,2,1,6,5,4)
vmat <- data.frame(v1,v2,v3,v4,v5,v6)
m1 <- cbind(v1,v2,v3,v4,v5,v6)
  # vytvoreni dat
# Zakladem analyzy hlavnich komponent je korelacni matice
cor(m1)
  # korelacni matice
eigen(cor(m1))
  # vlastni cisla a vlastni vektory korelacni matice
screeplot(princomp(m1),type="l")
abline(h=1,col="green")
# Pocet hlavnich komponent
  # dostatecne velke procento vyuzite variability (80%)
  cumsum(eigen(cor(m1))$values/sum(eigen(cor(m1))$values))
    # prvni 3 komponenty vysvetli pres 90% variability
  # pocet vlastnich cisel vetsich nez 1
prcomp(m1)
(PC<-prcomp(vmat,scale=T))
  # hlavni komponenty
  # vrati variabilitu hlavnich komponent spolu s koeficienty jednotlivych komponent
plot(PC)
  # vykresli variabilitu
plot(PC$x[,1],PC$x[,2],pch=19,main="Prvni 2 hlavni komponenty")
  # vykresleni prvnich dvou hlavnich komponent, ukazuji v datech skupiny
# maji hlavni komponenty prirozenou interpretaci?
#   mnohdy ne, pak je potreba pouzit faktorovou analyzu

## Faktorova analyza
factanal(m1, factors=3) 
  # faktorova analyza jen prerotuje hlavni komponenty
  #	metoda rotace 'varimax' je brana jako zakladni (defaultni) 
  # vypis loadingu a procent vysvetlene variability
(sc<-factanal(~v1+v2+v3+v4+v5+v6, factors = 3,scores = "Bartlett")$scores)
  # faktorove skory pro jednotliva pozorovani
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

## Kanonicka korelace
#	korelace mezi dvema skupinami promennych
# pracujme s charakteristikami statu: osobni uspory, podil populace do 15 let,
#	podil populace nad 75 let, prijem na obyvatele, narust prijmu na obyvatele
# rozdelime promenne do skupin: populacni podily, ekonomicke charakteristiky
pop <- LifeCycleSavings[, 2:3]
oec <- LifeCycleSavings[, -(2:3)]
cancor(pop, oec)
# vypocet kanonickych korelaci
#	na vystupu jsou kanonicke korelace (jejich pocet je stejny jako 
#	pocet promennych v mensi skupine), koeficienty kanonickych promennych
#	prumery promennych

library(MASS)

## Diskriminacni analyza
Iris <- data.frame(rbind(iris3[,,1], iris3[,,2], iris3[,,3]),Sp = rep(c("s","c","v"), rep(50,3)))
# databaze o trech druzich kosatcu: Setosa (s), Versicolour (c), Virginica (v)
# mereny jsou 4 ukazatele: sepal length & width, petal length & width
#	kalisni a okvetni listek, vzdy delka a sirka
train <- sample(1:150, 75)
# nahodny vyber 75 rostlin z cele databaze
table(Iris$Sp[train])
# vstupni data do diskriminacni analyzy ... rostliny, u nichz presne zname druh
(z <- lda(Sp ~ ., Iris, prior = c(1,1,1)/3, subset = train))
# linearni diskriminacni analyza
# vystup: vstupni (apriori) pravdepodobnosti ... jake je ocekavane zastoupeni skupin v populaci 
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



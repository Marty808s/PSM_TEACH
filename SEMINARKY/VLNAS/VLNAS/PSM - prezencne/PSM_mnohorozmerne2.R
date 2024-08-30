###########################
library(MASS)
  # knihovna s nastroji mnohorozmerne statistiky

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
predict(z, Iris[-train, ])$x
  # vysledne hodnoty diskriminacnich funkci
predict(z, Iris[-train, ])$posterior
  # pravdepodobnosti zarazeni do jednotlivych populaci
predict(z, Iris[-train, ])$class
  # na zaklade vytvorene klasifikacni funkce priradi nova mereni do skupin
  #	vybere idealni skupinu + vypocte pravdespodobnosti s nimiz do jednotlivych skupin patri
table(Iris[-train,"Sp"],predict(z, Iris[-train, ])$class)
  # klasifikacni tabulka, jak dobre se trefil: v radcich skutecne hodnoty, ve sloupcich predikce

#########################
### Samostatne

# vyzkousejte si na datech z bipsie
data("biopsy")
  # jak vypada diskriminacni funkce rozlisujici zhoubny a nezhoubny nador

#########################
## Shlukova analyza

# budem delit americke staty do skupin na zaklade 4 ukazatelu: vrazdy, napadeni, populace, znasilneni
hc <- hclust(dist(USArrests), "ave")
  # hierarchicke clusterovani metodou average linkage
  # vstupem je matice vzdalenosti jednotlivych bodu
plot(hc, hang = -1)
  # nakresleni dendrogramu - postup, jak shlukuje
  #	nejprve ma kazde pozorovani svou vlastni skupinu, a ty se pak spojuji do vetsich celku
  #	mozny je i obraceny postup, tj. od jedne velke skupiny k mnoha malym
seg<-cutree(hc,k=4)
  # rozdeli data do 4 skupin
rect.hclust(hc, k=4, border="red")
  # mohu si nechat zobrazit skupiny do dendrogramu

# Jak vypadaji segmenty v datech?
  # v datech mam jen 4 promenne, mohu si nechat vykreslit
plot(USArrests$Murder,USArrests$Assault,col=seg,pch=19)
plot(USArrests$UrbanPop,USArrests$Assault,col=seg,pch=19)
plot(USArrests$Rape,USArrests$Assault,col=seg,pch=19)
  # pro segmentaci je klicova promenna Assault - proc?

USArrests.sc<-scale(USArrests)
  # spocitame standardiyovane promenne
hc.sc <- hclust(dist(USArrests.sc), "ave")
plot(hc.sc, hang = -1)
  # zde jsou videt 2 velke skupiny nebo 5 mensich
seg.sc<-cutree(hc.sc,k=5)
  # rozdeli data do 5 skupin
table(seg,seg.sc)
  # dame-li vsem promennym stejnou vahu, rozdeli se mi staty jinak

plot(USArrests$Murder,USArrests$Assault,col=seg.sc,pch=19)
plot(USArrests$UrbanPop,USArrests$Rape,col=seg.sc,pch=19)
  # zde je videt odlehla Aljaska

# vykresleni skupin v prvnich dvou hlavnich komponentach
pc<-prcomp(USArrests,scale=T)$x
plot(pc[,1],pc[,2],col=seg.sc,pch=19)

# V praxi se casteji pouziva metoda complete linkage
hc.sc2 <- hclust(dist(USArrests.sc))
plot(hc.sc2, hang = -1)
    # zde mi dendogram jasne deli data na 4 skupiny
seg.sc2<-cutree(hc.sc2,k=4)
  # rozdeli data do 4 skupin
table(seg,seg.sc2)
table(seg.sc,seg.sc2)
  # kontrola, jak vznikle skupiny souhlasi s predchozimi delenimi

# Zakresleni aktualniho deleni do skupin
plot(USArrests$Murder,USArrests$Assault,col=seg.sc2,pch=19)
plot(USArrests$UrbanPop,USArrests$Rape,col=seg.sc2,pch=19)
plot(pc[,1],pc[,2],col=seg.sc2,pch=19)
  # nahlavnich komponentachvychazi pekne

# je mozne vysledne segmenty popsat pomoci puvodnich promennych
tapply(USArrests$Murder,as.factor(seg.sc2),mean)
tapply(USArrests$Assault,as.factor(seg.sc2),mean)
tapply(USArrests$UrbanPop,as.factor(seg.sc2),mean)
tapply(USArrests$Rape,as.factor(seg.sc2),mean)
  # pro vybranou promennou spocita prumery za jednotlive shluky

# K-means clustering
require(graphics)

seg.km <- kmeans(USArrests.sc, 4)
table(seg.sc2,seg.km$cluster)
  # jak vychazi metoda K-means v porovnani s hierarchickym shlukovanim
plot(USArrests$Murder,USArrests$Assault,col=seg.km$cluster,pch=19)
plot(USArrests$UrbanPop,USArrests$Rape,col=seg.km$cluster,pch=19)
plot(pc[,1],pc[,2],col=seg.km$cluster,pch=19)
  # rozlozeni do skupin je velmi podobne
seg.km$centers
  # vidime stredy shluku u standardizovanych promennych

#########################
### Samostatne

# vyzkousejte si na datech o krabech
data("crabs")
  # segmentujte na zaklade ciselnych promennych (4.-8. sloupec)
  # nebyla by lepsi segmentace na zaklade hlavnich komponent?

#########################

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

#########################
### Samostatne

# Souvisi zasah policie s kriminalitou? 
#   Zasah policie popisuji promenne Po1 a Po2 a kriminalitu promenne Prob, Time a y.

# Souvisi nezamestnanost s kriminalitou?
#   Nezamestnanost popisuji promenne U1 a U2.

#########################
## Korespondencni analyza
#	zpusob, jak graficky znazornit vztah mezi dvema kategorickymi promennymi
library(ca)
data("author")
  # prodej knih v knihkupectvich
fit<-ca(author)
print(fit)			# zakladni vystupy
  # chi-kvadrat, inertia, souradnice
summary(fit) 		# dalsi vystupy
plot(fit) 			# zakladni graf
  # body, ktere jsou k sobe blizko si jsou podobne
plot(fit, mass = TRUE, contrib = "absolute", map ="rowgreen", arrows = c(FALSE, TRUE)) 
  # graf se sipkama

data(smoke)
  # fiktivni data o koureni ve firme
fit2 <- ca(smoke)
plot(fit2)
  # oddeleni SE kouri nejmene a JM naopak nejvice
summary(fit2)

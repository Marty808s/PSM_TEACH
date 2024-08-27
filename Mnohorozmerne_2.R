# knihovna s nastroji mnohorozmerne statistiky
library(MASS)

## Diskriminacni analyza
iris <- iris3

df <- data.frame(rbind(iris[,,1],iris[,,2],iris[,,3]),Sp = rep(c("s","c","v"), rep(50,3)))
df
len <- length(df[,1])
len

# Train indexy - 50% z df
train_index <- sample(1:len,0.5*len)

# Počty jednotlivých skupin
table(df$Sp[train_index])

#Model lda
# Sp => závislá proměnná z df
# Sp ~ . <- značí že budou použity všechny prediktory
# df <- datový rámec
# prior <- apriorní pravděpodobnosti pro jedotlivé třídy
# subset <- index řádků k trénování modelu

model <- lda(Sp ~ ., df, prior=c(1,1,1)/3, subset = train_index)

predpoved <- predict(model, df[-train_index,])

predict(model, df[-train_index,])$x #<- vysledky diskriminačni funkci

predict(model, df[-train_index,])$posterior #<- Pst. pro zařazení do skupin

hat_skupiny <- predict(model, df[-train_index, ])$class #<- zařazené kupiny

# Klasifikačí tabulka -> validace 
table(df[-train_index,]$Sp,hat_skupiny)

error <- 3/75
accuracy = 1-error  
accuracy

# df pro vizualizaci
df_vystup <- cbind(df,hat_skupiny)
df_vystup

plot(predpoved$x[,1],predpoved$x[,2], pch=19, col=predpoved$class)
#-----------------------------------------------------

## Shlukova analyza

data <- USArrests
data

# Vypočíétám vzdálenosti jednotlivých proměnách
data <- dist(data)

hc <- hclust(data, "ave") #<- ave je metoda

# Dendogram
plot(hc, hang=-1) #<- hang -> prodlouží "nožičky"

# Cut ve stromu
seg <- cutree(hc,k=4)

# Zobrazeni skupin do dendogramu
rect.hclust(hc, k=4, border="red")

# Segmenty v datech
plot(USArrests$Murder,USArrests$Assault, col=seg, pch=19)
plot(USArrests$Murder,USArrests$UrbanPop, col=seg, pch=19)
plot(USArrests$Rape,USArrests$Assault, col=seg, pch=19)

#Škálování dat
USArrests.sc <- scale(USArrests)
hc.sc <- hclust(dist(USArrests.sc), "ave")
plot(hc.sc, hang = -1)
seg.sc<-cutree(hc.sc,k=5)

# Porovnání rozdělení
table(seg,seg.sc)

plot(USArrests$Murder,USArrests$Assault,col=seg.sc,pch=19)
plot(USArrests$UrbanPop,USArrests$Rape,col=seg.sc,pch=19)

# Vykreslení hlaních komponent
pc <-prcomp(USArrests.sc)$x
pc

cumsum(eigen(cor(USArrests.sc))$values/sum(eigen(cor(USArrests.sc))$values))
# => skupiny by měly být 2

# V praxi se casteji pouziva metoda complete linkage -> metoda
hc.sc2 <- hclust(dist(USArrests.sc))
plot(hc.sc2, hang = -1)
rect.hclust(hc.sc2, k=4, border="red")

seg.sc2<-cutree(hc.sc2,k=4)
table(seg,seg.sc2)
table(seg.sc,seg.sc2)

# Zakresleni aktualniho deleni do skupin
plot(USArrests$Murder,USArrests$Assault,col=seg.sc2,pch=19)
plot(USArrests$UrbanPop,USArrests$Rape,col=seg.sc2,pch=19)

# je mozne vysledne segmenty popsat pomoci puvodnich promennych
tapply(USArrests$Murder,as.factor(seg.sc2),mean)
tapply(USArrests$Assault,as.factor(seg.sc2),mean)
tapply(USArrests$UrbanPop,as.factor(seg.sc2),mean)
tapply(USArrests$Rape,as.factor(seg.sc2),mean)
# pro vybranou promennou spocita prumery za jednotlive shluky


#-----------------------------------------------------
###
#K-means clustering
require(graphics)

seg.km <- kmeans(USArrests.sc, 4)
table(seg.sc2,seg.km$cluster)

plot(USArrests$Murder,USArrests$Assault,col=seg.km$cluster,pch=19)
plot(USArrests$UrbanPop,USArrests$Rape,col=seg.km$cluster,pch=19)
plot(pc[,1],pc[,2],col=seg.km$cluster,pch=19)
seg.km$centers

#-----------------------------------------------------
### #K-means clustering - Crabs
data("crabs")
data <- crabs[,4:8] #manipulace s daty 4-8 sloupec
data

# Kmeans -4
crabs.km <- kmeans(data,4)

#Hierarchické clustering
crabs.hcl <- hclust(dist(data))

plot(crabs.hcl)
rect.hclust(crabs.hcl, k=4, border="red")

# mám rozdělení do 4 skupin
crabs.reg <- cutree(crabs.hcl, k=4)

table(crabs.reg,crabs.km$cluster)

## Analýza hlavních komponent
pca <- prcomp(data)
summary(pca) #tady lze vidět, že první komponenta vysvětluje 98%, takže stačí jedna proměnná

factanal(data, factors = 2)
cumsum(eigen(cor(data))$values/sum(eigen(cor(data))$values))

data_filtered <- data [,1] #<- vezmeme jenom jendu komponentu
cluster2<- kmeans(data_filtered, 2)
cluster3<-hclust(dist(data_filtered))
cl3.cut <- cutree(cluster3, k=2)

plot(cluster3)
rect.hclust(cluster3, k=2, border="red")

table(cl3.cut,cluster2$cluster)

####################################
#Druhý pokus:
data("crabs")

#Standardizace numerických proměnných
crabs_scaled <- scale(crabs[, 4:8])

#Hierarchické shlukování
hc_crabs <- hclust(dist(crabs_scaled), "ave")
plot(hc_crabs, hang = -1)

#K-means shlukování
km_crabs <- kmeans(crabs_scaled, centers = 3)  # Zvolíme 3 shluky jako příklad
plot(crabs_scaled[, 1], crabs_scaled[, 2], col = km_crabs$cluster, pch = 19)

#PCA
pca_crabs <- prcomp(crabs_scaled, scale = TRUE)
plot(pca_crabs$x[, 1], pca_crabs$x[, 2], col = km_crabs$cluster, pch = 19)

#-----------------------------------------------------

## Kanonicka korelace
#	korelace mezi dvema skupinami promennych
# pracujme s charakteristikami statu: osobni uspory, podil populace do 15 let,
#	podil populace nad 75 let, prijem na obyvatele, narust prijmu na obyvatele
# rozdelime promenne do skupin: populacni podily, ekonomicke charakteristiky
pop <- LifeCycleSavings[, 2:3]
oec <- LifeCycleSavings[, -(2:3)]
cancor(pop, oec)

#-----------------------------------------------------

## Korespondencni analyza
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



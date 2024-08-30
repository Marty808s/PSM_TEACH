library(MASS)
data(UScereal)
??USCereal
attach(UScereal)

#--------------------------------------
#1. Otestujte, zda se liší výrobky jednotlivých výrobců (proměnná mfr) co se složení týče (pracujte se všemi číselnými proměnnými). Pokud ano, najděte vhodné rozhodovací funkce, které Vám výrobky podle složení k jednotlivým výrobcům přiřadí. Jak spolehlivé takové rozhodování je?
#  (využijte MANOVU a diskriminační analýzu)

# vytvořím si data
vyrobce <- UScereal$mfr 
kalorie <- UScereal$calories
protein <- UScereal$protein
fat <- UScereal$fat
sod <- UScereal$sodium
fib <- UScereal$fibre
carb <- UScereal$carbo
sug <- UScereal$sugars
pot <- UScereal$potassium

# vytvořím tabulku s číselnými proměnnými
tab <- cbind(kalorie, protein, fat, sod, fib, carb, sug, pot)

#manova
#h0: mezi jednotlivými výrobky neexistují rozdíly x h1: existují rozdíly
fit <- manova(tab ~ vyrobce)
#sumář
summary(fit)
# 0.008897 < 0.05 = zamítám h0, najdeme rozdíl alespoň u jednoho výrobku
# podívám se podrobněji na jednotlivé proměnné pro identifikaci závislostí
summary.aov(fit)
#Response fib :
#Df Sum Sq Mean Sq F value  Pr(>F)  
#vyrobce      5  479.5  95.899  2.9345 0.01969 *

#provedu diskrimanační analýzu
#pro zajištění lepšího výsledku škáluju data
scaled_tab <- scale(tab)

apply(scaled_tab, 2, mean)
#žádná proměnná není značně vzdálená od 0, tzn data by měla být sprtávně normalizována
apply(scaled_tab, 2, sd)
#odchylky vychází všude jako 1, značí správnou normalizaci
#vytvořím si z dat datový rámec
tab_df <- as.data.frame(tab)
#přidám sloupeček s výrobci
tab_df$vyrobce <- UScereal$mfr
#rozdělím si data v poměru 60/40 na tréninková a testovací
vzorek <- sample(c(TRUE, FALSE), nrow(tab_df), replace=TRUE, prob=c(0.6,0.4))
train_data <- tab_df[vzorek, ]
test_data <- tab_df[!vzorek, ] 

#diskriminační analýza
lda_train <- lda(vyrobce~., data=train_data)
print(lda_train)
#Prior probabilities of groups:
#  G          K          N          P          Q          R 
#0.33333333 0.33333333 0.07142857 0.14285714 0.07142857 0.04761905 
#Group means:
#  kalorie  protein       fat      sod       fib     carb       sug       pot
#G 130.4762 2.785714 1.4285714 230.7143  1.833333 17.45238  9.642857 114.52381
#K 138.9641 3.311212 0.5817342 207.5107  4.049307 20.34239  9.324360 136.95096
#N 160.2593 7.025479 1.0101010 131.3131 13.583597 24.45349  6.060606 412.18152
#P 226.1134 5.430612 2.4242801 256.0510  5.327115 28.69720 12.962310 189.48609
#Q 106.4179 2.656716 1.6616915 147.9602  1.328358 14.30348  6.651741  67.26368
#R 122.1642 2.492537 0.7462686 289.2537  2.985075 22.19403  5.977612 105.78358

#největší zastoupení mají výrobci G a K, dále vidíme průměrné hodnoty jejich výrobků

#hodnoty jednotlivých diskriminačních funkcí na testovacích datech
predict(lda_train, test_data)$x
#posteriorní pravděpodobnosti pro každou třídu pro každé pozorování v testovacích datech
predict(lda_train, test_data)$posterior
#předpovězené třídy pro každé pozorování v testovacích datech
predict(lda_train, test_data)$class

#vytvoření tabulky skutečných a předpovězených hodnot
test_data$class <- as.factor(test_data$vyrobce)
table(test_data$class, predict(lda_train, newdata = test_data)$class)
#výpočet přesnosti modelu
prediction <- predict(lda_train, test_data)
mean(prediction$class==test_data$vyrobce)
#přesnost modelu je 0.3478261 - značně nepřesný model, předpověděl správně 34,8% případů
#vizalizace diskriminačních funkcí
plot(prediction$x[,1],prediction$x[,2],pch=19,col=prediction$class,
     main="Graf diskriminacnich funkci",xlab="LD1",ylab="LD2")
legend(9,2.2,legend=c(unique(prediction$class)),pch=19,col=1:3)


#DOPLNĚNÍ
#Porovnání MANOVy a Diskriminační analýzy
#MANOVA může odhalit statisticky významné rozdíly mezi skupinami, i když tyto rozdíly nejsou natolik výrazné,
#aby umožnily úspěšnou klasifikaci při použití diskriminační analýzy. Na druhou stranu diskriminační analýza může selhat,
#pokud se skupiny výrazně překrývají.

#--------------------------------------
#2. Pokuste se data zjednodušit, tedy snížit počet proměnných tak, abyste udrželi co možná nejvíce informace (variability) a přitom bylo možné data co nejjednodušeji graficky znázornit. Je možné proměnné rozdělit do skupin (tedy najít mezi nimi vhodný počet interpretovatelných faktorů)?
#(využijte faktorovou analýzu, případně metodu hlavních komponent)

#pro metodu hlavních komponent si vytvoříme korelační matiic
corel <- cor(scaled_tab)
res_eigen <- eigen(corel)
#pro náhled na jednotlivé komponenty si vytvoříme screeplot
screeplot(princomp(scaled_tab),type="l")
abline(h=1,col="blue")
#z grafu je patrné, že první 3 komponenty budou hlavní
cumsum(res_eigen$values/sum(res_eigen$values))
#z výpočtu variability vidíme, že první 3 komponenty obsahují 84,53144% variability
# 0.5301827 0.6982353 0.8453144 0.9212016 0.9825183 0.9954659 0.9991124 1.0000000
#faktorová analýza
res_factanal <- factanal(scaled_tab, factors = 3)
#přerotování hlavních komponent pro lepší interpretaci
(sc<-factanal(~scaled_tab, factors = 3, scores = "Bartlett")$scores)
res_factanal$loadings
#Loadings:
#         Factor1 Factor2 Factor3
#kalorie  0.300   0.775   0.551 
#protein  0.781   0.450   0.219 
#fat      0.177   0.190   0.695 
#sod      0.483   0.365   0.183 
#fib      0.979                 
#carb     0.144   0.986         
#sug                      0.874 
#pot      0.962   0.111   0.185
#factor1 = protein, fib, sod, pot
#factor2 = kalorie, carb
#factor3 = fat, sug
#DOPLNĚNÍ POJMENOVÁNÍ FAKTORŮ
plot(sc[,1], sc[,2], pch=19, main = "faktory 1 a 2", xlab = "Nutriční složky", ylab= "Energetický obsah")
plot(sc[,1], sc[,3], pch=19, main = "faktory 1 a 3", xlab = "Nutriční složky", ylab= "Tuky a cukry")
plot(sc[,2], sc[,3], pch=19, main = "faktory 2 a 3", xlab = "Energetický obsah", ylab= "Tuky a cukry")
#                  Factor1 Factor2 Factor3
#SS loadings       2.88   1.964   1.673
#Proportion Var    0.36   0.245   0.209
#Cumulative Var    0.36   0.605   0.815
#první 2 faktory nám tedy vysvětlují 60,5% variability
#--------------------------------------
#3. Rozdělte výrobky do vhodných skupin.
#(využijte shlukovou analýzu)

#install.packages("cluster")
#install.packages("factoextra")
#library(cluster)
#library(factoextra)

#normalizace dat prvních 2 faktorů
data <- tab[,c("protein","fib","sod","pot","kalorie","carb")]
data_scaled <- scale(na.omit(data))
#funkce pro výpočet koeficientu aglomerace pro danou metodu
methods <- c("average", "single", "complete", "ward")
agl_coef <- function(method) {
  agnes(data_scaled, method = method)$ac
}

#výpočet koeficientů aglomerace pro všechny metody
sapply(methods, agl_coef)
#average    single  complete      ward 
#0.8817115 0.8557202 0.9238024 0.9461144 
#nejvyšší koeficient má metoda ward, dosáhneme tím přesnějších výsledků při shlukové analýze
#výpočet vzdálenostní matice pomocí euklidovské vzdálenosti
distance_matrix <- dist(data_scaled, method = "euclidean")
#provedení hierarchické shlukové analýzy
hc <- hclust(distance_matrix, method = "ward.D2")
#vykreslení dendrogramu pro náhled jak se bude shlukovat
plot(hc, hand = -1)
#rozhodl jsem se rozdělit data na 5 skupin
seg <- cutree(hc, k = 5)
table(seg)
#zobrazení jednotlivých skupin do grafu
rect.hclust(hc, k = 5, border = "red")
#vykreslení skupin jednotlivých proměnných
plot(tab_df$protein,tab_df$kalorie,col=seg,pch=19)
plot(tab_df$fib,tab_df$kalorie,col=seg,pch=19)
plot(tab_df$sod,tab_df$kalorie,col=seg,pch=19)
plot(tab_df$pot,tab_df$kalorie,col=seg,pch=19)
plot(tab_df$carb,tab_df$kalorie,col=seg,pch=19)
#rozdělení do skupin podle průměrných hodnot
mean_val <- aggregate(data_scaled, by=list(seg), FUN=mean)
#Group.1    protein        fib         sod        pot    kalorie        carb
#1       1  2.6730194  4.0196942  1.90947778  3.6992961  0.4059999 -0.29675648
#2       2 -0.3257141 -0.3658140  0.13163987 -0.3994560 -0.2880970 -0.06324694
#3       3 -0.7783745 -0.5253257 -0.89489422 -0.6382252 -0.6985535 -0.87737534
#4       4  0.6142708  0.3318950 -0.04159999  0.5691366  0.8022952  0.59139190
#5       5  3.1469906  1.3253906  3.38484966  1.1142150  4.6560329  5.67190886
# DOPLNĚNÍ: 
# Group 1 : Vyvážené cereálie
# Protein: 2.6730194
# Fib: 4.0196942
# Sod: 1.90947778
# Pot: 3.6992961
# Kalorie: 0.4059999
# Carb: -0.29675648

# Group 2 : Nízkokalorické cereálie
# Protein: -0.3257141
# Fib: -0.3658140
# Sod: 0.13163987
# Pot: -0.3994560
# Kalorie: -0.2880970
# Carb: -0.06324694

# Group 3 : Cereálie s nízkým obsahem živin
# Protein: -0.7783745
# Fib: -0.5253257
# Sod: -0.89489422
# Pot: -0.6382252
# Kalorie: -0.6985535
# Carb: -0.87737534

# Group 4 : Cereálie se zvýšeným obsahem bílkovin a kalorií
# Protein: 0.6142708
# Fib: 0.3318950
# Sod: -0.04159999
# Pot: 0.5691366
# Kalorie: 0.8022952
# Carb: 0.59139190

# Group 5 : Cereálie s vysokou energetickou hodnotou
# Protein: 3.1469906
# Fib: 1.3253906
# Sod: 3.38484966
# Pot: 1.1142150
# Kalorie: 4.6560329
# Carb: 5.67190886

res_kmeans <- kmeans(data_scaled, centers = 5)
res_kmeans
plot(tab_df$protein, tab_df$kalorie, col=res_kmeans$cluster, main = "Metoda k-means", pch=19)
plot(tab_df$fib, tab_df$kalorie, col=res_kmeans$cluster, main = "Metoda k-means", pch=19)
plot(tab_df$sod, tab_df$kalorie, col=res_kmeans$cluster, main = "Metoda k-means", pch=19)
plot(tab_df$pot, tab_df$kalorie, col=res_kmeans$cluster, main = "Metoda k-means", pch=19)
plot(tab_df$carb, tab_df$kalorie, col=res_kmeans$cluster, main = "Metoda k-means", pch=19)

res_kmeans$size
#oproti shlukové analýze je patrné rovnoměrnější rozdělení pomocí metody k-means
#seg 1  2  3  4  5  x  1 2   3   4   5
#    3 31 14 16  1     2 24  19  17  3

res_kmeans$centers
#   protein    fib        sod         pot        kalorie    carb
#1  2.5965724  1.0882392  1.65198364  0.9562192  4.0442612  3.98293401
#2 -0.5322774 -0.4153564 -0.63067044 -0.4945419 -0.6183065 -0.72844292
#3  0.4835370  0.2116359  0.01857239  0.4188561  0.5462947  0.46478540
#4 -0.5661617 -0.4875347  0.33828321 -0.5352699 -0.2851039  0.09271229
#5  2.6730194  4.0196942  1.90947778  3.6992961  0.4059999 -0.29675648
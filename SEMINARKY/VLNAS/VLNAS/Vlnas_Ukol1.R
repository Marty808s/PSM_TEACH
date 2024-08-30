library(MASS)
data(UScereal)
attach(UScereal)
UScereal

#############################################################################

#1. Otestujte, zda se liší výrobky jednotlivých výrobců (proměnná mfr) 
#co se složení týče (pracujte se všemi číselnými proměnnými). Pokud ano, 
#najděte vhodné rozhodovací funkce, které Vám výrobky podle složení k jednotlivým 
#výrobcům přiřadí. Jak spolehlivé takové rozhodování je?
#  (využijte MANOVU a diskriminační analýzu)

# a) Použití MANOVA testu
#H0: Vektory středních hodnot se rovnají => tím pádem neexistují významné rozdíli mezi výrobky
#H1: Vektory středních hodnot se liší => takže existují významné rozdíli mezi výrobky

# Získáme číselné proměnné z datasetu
prom1 <- cbind(cal <- UScereal$calories,
               protein <- UScereal$protein,
               fat <- UScereal$fat,
               sodium <- UScereal$sodium,
               fibre <- UScereal$fibre,
               carbo <- UScereal$carbo,
               sugar <- UScereal$sugars,
               potass <- UScereal$potassium)

# Provedení manova testu
manova_test <- manova(prom1 ~ mfr, data = UScereal)
summary(manova_test)
# Vyhodnocení testu:
#   p-value = 0.008897, alpha = 0.05
#   p-value < 0.05 -> tím pádem zamítáme H0 a platí alternativa H1
# Tím pádem existuje alespoň jeden výrobek, který se výrazně liší od těch ostatních.

# Výsledky testů pro jednotlivé proměnné
summary.aov(manova_test)
# Tam je očividné, žě složení - Response 5 [fibre/vlákniny] je rozdílé vůči nezávislé proměné (vůči jiných výrobců)

#------------------------------------------------------------------------------

# b) Diskriminační analýza

# Škálování dat
prom1_scaled <- scale(prom1)
# Data nejsou vzdálena od 0 - jsou tedy správně normalizovaná
apply(prom1_scaled, 2, mean)
apply(prom1_scaled , 2, sd)
# Tvorba datového rámce
df <- as.data.frame(prom1)
# Přidání sloupce výrobců
vyrobce <- UScereal$mfr 
df$vyrobce <- vyrobce
# Testovací x Trénovací data - rozložení dat - indexace
vzorek <- sample(c(TRUE, FALSE), nrow(df), replace=TRUE, prob=c(0.6,0.4))

# Data - z df vytvořím jednotlivé datasety
train <- df[vzorek,]
test <- df[!vzorek,]  

# Diskriminační analýza
lda_train <- lda(vyrobce~., data = train)
print(lda_train)

# Rozdělení zastoupení jednotlivých výrobců
#     G          K          N          P          Q          R 
#0.28947368 0.34210526 0.07894737 0.18421053 0.07894737 0.02631579 

# --> největší zastoupení má výrobce G a K - nejnižší zase R

# Průměr nutričních hodnot daných výrobců
#     V1       V2       V3       V4        V5       V6        V7        V8
#G 154.8485 3.363636 2.272727 251.2121  2.212121 18.09091 11.939394 136.81818
#K 163.0105 3.850885 1.321122 244.6321  4.657412 19.40017 13.889573 186.93230
#N 160.2593 7.025479 1.010101 131.3131 13.583597 24.45349  6.060606 412.18152
#P 213.0012 5.294469 2.077954 264.2484  5.632196 27.36945 12.176649 202.92838
#Q 136.6667 3.444444 1.555556 187.7778  1.333333 19.00000  9.333333  93.88889
#R 133.3333 2.666667 0.000000 253.3333  1.333333 24.00000  6.666667 106.66667

# Hodnoty diskriminačních funkcí
predict <- predict(lda_train, test)
predict$x
# Predikovaná pravděpodobnost pro každou třídu v testovacích datech - na jeden záznam
predict$posterior
# Výsledná předpověď skupiny pro daný záznam - pouze ta patřičná :)
predict$class

# Vizualizace předpovědi
test$class <- as.factor(test$vyrobce)
tab <- table(test$class, predict$class)
tab

# Výstup z tabulky predikovaných a skutečnác skupin:
#   G K N P Q R
# G 7 1 0 0 0 0
# K 3 4 0 0 0 0
# N 0 1 1 0 0 0
# P 1 0 0 0 0 0
# Q 1 0 0 0 1 0
# R 1 0 0 0 1 0

# => [G] Skupina G byla modelována dobře s vysokou přesností; došlo pouze k jedné chybné klasifikaci, kde produkt byl přiřazen ke skupině K.
# => [K] U skupiny K byla přesnost nižší s třemi produkty nesprávně klasifikovanými ke skupině G, 
  # což může být způsobeno podobností nutričních profilů těchto dvou skupin.
# => U ostatních skupin (N, P, Q, R) lze pozorovat obtížnější klasifikaci, což je pravděpodobně způsobeno nízkým počtem vzorků v datasetu. 
  # Proto je obtížnější a nepřesnější kvalifikace..
# Celkově pro lepší přesnost modelu by bylo nutné zvýšit počet dat

# Přesnost modelu
mean(test$class==predict$class)
# => výsledek - přesnost modelu vyšla: 0.4814815 -> což je velice nízké... :(
# - model předpověděl správně jen 48% případů... 

# Porovnání MANOVA X Diskriminační analýza
# MANOVA může ukazovat statisticky významné rozdíly mezi skupinami i v případě, 
# že tyto rozdíly nejsou dostatečně velké nebo charakteristické pro úspěšnou klasifikaci v diskriminační analýze (viz. tabulka skupin nahoře). 
# Diskriminační analýza může selhat, pokud jsou skupiny příliš překrývající.

#############################################################################

#2. Pokuste se data zjednodušit, tedy snížit počet proměnných tak, abyste udrželi co možná nejvíce informace (variability) 
#a přitom bylo možné data co nejjednodušeji graficky znázornit. Je možné proměnné rozdělit do skupin (tedy najít mezi nimi vhodný počet 
#interpretovatelných faktorů)?
# (využijte faktorovou analýzu, případně metodu hlavních komponent)

# Metoda hlavích komponent
#   Tvorba korelační matice:
cor <- cor(prom1_scaled)
res_eigen <- eigen(cor)
res_eigen

# Vizuální náhled hl. komponent pomocí screeplotu
screeplot(princomp(prom1_scaled), type="l")
abline(h=1, col="red")
# => z grafu lze poznat, že první 3 komponenty budou hlavní! - nachází se nad hladinou v grafu

# Výpočet variability - pro lepší identifikaci, které komponenty jsou hlavní
cumsum(res_eigen$values/sum(res_eigen$values))
# 0.5301827 0.6982353 0.8453144 0.9212016 0.9825183 0.9954659 0.9991124 1.0000000
# => první 3 komponenty obsáhnou 84,5% variability dat - proto jsou tedy hlavní komponenty

# Faktorová analýza
res_fact <- factanal(prom1_scaled, factors = 3)
res_fact$loadings

#         Factor1 Factor2 Factor3
#kalorie  0.300   0.775   0.551 
#protein  0.781   0.450   0.219 
#fat      0.177   0.190   0.695 
#sod      0.483   0.365   0.183 
#fib      0.979                 
#carb     0.144   0.986         
#sug                      0.874 
#pot      0.962   0.111   0.185
#------------------------------------
# Identifikace nejvýznamějších komponent faktorů + jejich pojmenování
#factor1 = protein, fib, sod, pot = faktor "Nutriční hodnota"
#factor2 = kalorie, carb = faktor "Energetický obsah"
#factor3 = fat, sug = faktor "Palatabilita"

# Factor1 Factor2 Factor3
# SS loadings       2.88   1.964   1.673
# Proportion Var    0.36   0.245   0.209
# Cumulative Var    0.36   0.605   0.815

#   => první dva faktory pokryjí 60.5%  (s třetím 81,5%) variability dat

sc <- factanal(~prom1_scaled, factors = 3, scores = "Bartlett")$scores
sc

# Vizualizace faktorů - porovnání jednotlivých faktorů
plot(sc[,1], sc[,2], pch=19, main = "první x druhý", xlab = "faktor1", ylab = "faktor 2")
plot(sc[,1], sc[,3], pch=19, main = "první x třetí", xlab = "faktor1", ylab = "faktor 2")
plot(sc[,2], sc[,3], pch=19, main = "druhý x třetí", xlab = "faktor1", ylab = "faktor 2")

#############################################################################

#3. Rozdělte výrobky do vhodných skupin.
#(využijte shlukovou analýzu)

#install.packages("cluster")
#install.packages("factoextra")
library(cluster)
library(factoextra)

# Příprava datasetu
cal <- UScereal$calories
protein <- UScereal$protein
fat <- UScereal$fat
sodium <- UScereal$sodium
fibre <- UScereal$fibre
carbo <- UScereal$carbo
sugar <- UScereal$sugars
potass <- UScereal$potassium

# Normalizace dat u prvních dvou významných faktorů
data <- cbind(cal,protein,fibre,sodium,potass,carbo)
scaled_data <- scale(na.omit(data))

# Výpočet koeficientů aglomerace pro jednotlivé metody aglomerace
methods <- c("average", "single", "complete", "ward")

agl_coef <- function(method) {
  agnes(scaled_data, method = method)$ac
}

# Výstup
sapply(methods, agl_coef)

#  average    single  complete    ward 
#0.8817115 0.8557202 0.9238024 0.9461144 
# => z výstupu jednotlivých metod je očividné, že nejpřesnějším výsledkem
#     shlukové analýzy bude metoda ward, která má nejvetší koeficient

# Výpočet vzdálenostní matice pomocí euklidovské metody (euklidovské vzdálenosti)
dist_m <- dist(scaled_data, method="euclidean")

# Hierarchická shluková analýza
hc <- hclust(dist_m, method = "ward.D2")

# Vizualizace dendogramu -> získáme představu jak budeme následně shlukovat do skupin
plot(hc, hang = -1)
#   -> po vizualizaci jsem se rozhodl, že rozložíme data do 5ti skupin

cut <- cutree(hc, k=5)
table(cut) #<- vizualizace četnosti v jednotlivých skupinách

# skupina: 1  2  3  4  5 
# četnost: 3 31 14 16  1 

# Vizualizace skupin graficky
rect.hclust(hc, k = 5, border="red")

# Vykreslení jednotlivých proměnných
plot(df$V2, df$V1, col=cut, pch=19)
plot(df$V3, df$V1, col=cut, pch=19)
plot(df$V4, df$V1, col=cut, pch=19)
plot(df$V5, df$V1, col=cut, pch=19)
plot(df$V6, df$V1, col=cut, pch=19)

# Rozdělení do skupin podle mean hodnot
mean_val <- aggregate(scaled_data, by=list(cut), FU=mean)
mean_val

# Group.1        cal    protein      fibre      sodium     potass       carbo
# 1       1  0.4059999  2.6730194  4.0196942  1.90947778  3.6992961 -0.29675648
# 2       2 -0.2880970 -0.3257141 -0.3658140  0.13163987 -0.3994560 -0.06324694
# 3       3 -0.6985535 -0.7783745 -0.5253257 -0.89489422 -0.6382252 -0.87737534
# 4       4  0.8022952  0.6142708  0.3318950 -0.04159999  0.5691366  0.59139190
# 5       5  4.6560329  3.1469906  1.3253906  3.38484966  1.1142150  5.67190886
# Souhrn a popis jedotlivých skupin
  # 1. skupina - hodně protein, vláknina, draslík
  # 2. skupina - všeho dost - neutrálí (mírná převaha sodíku)
  # 3. skupina - dost podobné jako 2. - taky celkem neutrální
  # 4. skupina - vysoko kalorické, hodně bílkovin, sodíku, draslíku - celkově všeho hodně

# Metoda K-means pro následné porování
kmeans_result <- kmeans(scaled_data, centers=5, nstart=25)
plot(scaled_data, col=kmeans_result$cluster, main="K-means metoda", pch=20)

kmeans_result$size
kmeans_result$centers

# jednotlivé skupiny dle k-means
# 22  19  2  19  3

#       cal     protein      fibre      sodium     potass       carbo
#1 -0.6438185 -0.6030142 -0.4179755 -0.68602881 -0.4966633 -0.74133756
#2  0.5462947  0.4835370  0.2116359  0.01857239  0.4188561  0.46478540
#3  4.0442612  2.5965724  1.0882392  1.65198364  0.9562192  3.98293401
#4 -0.2906377 -0.4806891 -0.4769043  0.30038725 -0.5285263  0.02120553
#5  0.4059999  2.6730194  4.0196942  1.90947778  3.6992961 -0.29675648

# Z výsledků je patrné, že rozdělení četnosti pro K_means metodu je realitvně vyrovnanější.
# Z metody K means mi i výsledné skupiny dávají v kontextu větší smysl:
  # 1. skupina - nízkokalorické
  # 2. skupina - standart
  # 3. skupina - vysokokalorické
  # 4. skupina - mezi nízko a standart
  # 5. skupina - cereálie s extra proteinem a vlákniny - fitness?
#------------------------------------------------------------------------------
                      

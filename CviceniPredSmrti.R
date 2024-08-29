library(DescTools)
library(TeachingDemos)

data <- Kojeni2

# Závisí dosažené vzdělání na počet dětí?
# mám více jak 2 levely v kategorické prom, takže ANOVA ==> NE DVOUVÝBĚROVÝ TEST

vzdelani <- as.factor(data$Vzdelani)
pocet_deti <- data$pocet.deti

# Test normality residuí
res <- residuals(lm(pocet_deti~vzdelani))

shapiro.test(res)
# p-value = 3.055e-10 < alfa => NEMAJI normalni rozdeleni
#==> Kruskal...


# Test rozptylu
bartlett.test(pocet_deti~vzdelani)
# p-value = 0.6016 > alfa - nezamítáme H0 => mají shodé rozptyly

#H0: rozptyly pocet deti u jednotlivých vzdelani nelisi | H1: lisi
kruskal.test(pocet_deti~vzdelani)
#==> p-value = 0.3357 > alfa => nezamítám H0

boxplot(pocet_deti~vzdelani)
table(pocet_deti,vzdelani)

#---------------------------------------------------------------------------------

# Mohorozměrná analýza
library(DescTools)

# Liší se hmotnost, vyska, trvani se získaným vzděláním rodičů
vyska <- data$por.delka
hmot <- data$por.hmotnost
trvani <- data$trvani


# mám všechny číselné na jednu kategorii (ta má více jak 2 levely) -> MANOVA

datova <- cbind(vyska,hmot,trvani)


m <- manova(datova~factor(vzdelani))
summary.aov(m)

# POZNÁMKA: pro MANOVU by chtělo více faktorů - kat. proměných

#---------------------------------------------------------------------------------

# Metoda hlavních komponent
vyska <- data$por.delka
hmot <- data$por.hmotnost
trvani <- data$trvani
vysM <- data$vyskaM
vysO <- data$vyskaO

# Počet hlavních komponent
df <- cbind(vyska,hmot,trvani,vysM,vysO)
df

PC <- prcomp(df)

plot(PC)

screeplot(princomp(df),type="l")
abline(h=1,col="green")

cumsum(eigen(cor(df))$values/sum(eigen(cor(df))$values))


# Faktorová analýza
#==> vemu počet hl. komponent a podle nic vytvořím počet faktorů)
factanal(df, factor=2)

#---------------------------------------------------------------------------------

# Diskriminační analýza
library(MASS)

data <- Ichs
kour <- data[,8]

df <- data[,1:6]
df <- cbind(df,kour)

len <- length(df[,1])

train <- sample(1:len,0.5*len)
train 

model <- lda(df$kour ~ ., df, prior=c(1,1,1,1)/4, subset=train)

model

pred <- predict(model,df[-train,])

table(df[-train,]$kour,pred$class)

df_vystup <- cbind(df, pred$class)

plot(pred$x[,1],pred$x[,2], pch=19, col=pred$class)

#---------------------------------------------------------------------------------

# Shlukování
#1. Hierarchické
data <- USArrests
data

prcomp(data)

hc <- hclust(dist(data), "ave")
plot(hc, hang=-1)
rect.hclust(hc, k=3, border="red")

cut <- cutree(hc,3)

plot(data$Assault, data$UrbanPop, col=cut, pch=19)
plot(data$Assault, data$Murder, col=cut, pch=19)


# Porovnání po škálování dat
data.sc <- scale(data)

hc.sc <- hclust(dist(data.sc))
plot(hc.sc,hang=-1)
rect.hclust(hc.sc,k=3, border="green")
cut.sc <- cutree(hc.sc,3)

table(cut,cut.sc)

# Vykreslení hlaních komponent
pc <-prcomp(data.sc)
pc

plot(pc)
abline(h=1)


# KMeans
require(graphics)
seq.km <- kmeans(data.sc,3)
table(cut.sc,seq.km$cluster)
cumsum(eigen(cor(data.sc))$values/sum(eigen(cor(data.sc))$values))

#---------------------------------------------------------------------------------
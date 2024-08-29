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

#---------------------------------------------------------------------------------
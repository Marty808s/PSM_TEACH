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


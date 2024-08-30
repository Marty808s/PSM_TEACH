
#1. V datech Deti.RData najděte optimální model závislosti vitální kapacity plic dvanáctiletých dětí (proměnná X.nvk12 - měřená v procentech) 
  #na jejich pohlaví, na sportu, který dělají, na váze, výšce, BMI, systolickém a diastolickém tlaku ve dvanácti letech a na vitální kapacitě plic v jedenácti letech 
  #(proměnné pohlavi, sport, hmot12, vyska12, bmi12, syst12, diast12 a X.nvk11). Napište odhadnutý model, interpretujte jeden vybraný regresní koeficient (ne absolutní člen), 
    #ohodnoťte kvalitu modelu, zkontrolujte splnění předpokladů. 

#Odhadněte, jakou vitální kapacitu by měl mít ve dvanácti letech kluk, který dělá atletiku, 
  #váží 40 kg, měří 160 cm a v jedenácti letech měl vitální kapacitu 100%. Pokud bude potřeba, uvažujte u něj průměrný tlak. 

###############################################################################

# Manipulace s daty
vit12 <- Deti$X.nvk12
pohlavi <- Deti$pohlavi
sport <- Deti$sport
hmot12 <- Deti$hmot12
vyska12 <- Deti$vyska12
bmi12 <- Deti$bmi12
syst12 <- Deti$syst12
diast <- Deti$dias12
vit11 <- Deti$X.nvk11

promenne <- list(pohlavi = pohlavi, sport = sport, hmot12 = hmot12, vyska12 = vyska12,
                 bmi12 = bmi12, syst12 = syst12, diast = diast, vit11 = vit11)

# Procházení proměnných a vykreslení grafů - zkoumám vit12 s dalšímy parametry
for (nazev in names(promenne)) {
  plot(vit12 ~ promenne[[nazev]], pch = 19, main = paste("Závislost vit12 na", nazev))
}
# => určitě na vit. kapacitě má vliv sport (plavci mají váznamně vyšší hodnotu kapacity)

# Vytvoření lineárního modelu - maximílního modelu
model1 <- lm(X.nvk12 ~ pohlavi + sport + hmot12 + vyska12 + bmi12 + syst12 + dias12 + X.nvk11, data = Deti)

# Shrnutí celého lin. reg. modelu - neoptimálního!
summary(model1)

# Koeficient pro plavání je velice významný:
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  171.80939   97.41133   1.764   0.0792 .  
#pohlaviz      -1.74739    1.87113  -0.934   0.3514    
#sportlh       -2.66939    1.86199  -1.434   0.1532    
#sportplavani   5.69194    2.28622   2.490   0.0136 * 
#....................................................
# => to znamená, že děti které dělají plaváí, mají 5.692x větší, než ti co dělají atletiku (to šlo vidět i na grafu nahoře)

#Hledání optimálního modelu pomocí krokové regrese z maximálního modelu
step(lm(X.nvk12 ~ pohlavi + sport + hmot12 + vyska12 + bmi12 + syst12 + dias12 + X.nvk11, data = Deti))

#Start:  AIC=1044.87
#X.nvk12 ~ pohlavi + sport + hmot12 + vyska12 + bmi12 + syst12 + 
#  dias12 + X.nvk1

#Step:  AIC=1043.74
#X.nvk12 ~ pohlavi + sport + hmot12 + vyska12 + bmi12 + syst12 + 
#  X.nvk11

#Step:  AIC=1042.77 
#X.nvk12 ~ sport + hmot12 + vyska12 + bmi12 + syst12 + X.nvk11

#Step:  AIC=1042.42 #=> nejmenší hodnota AIC
#X.nvk12 ~ sport + hmot12 + vyska12 + bmi12 + X.nvk11

#   Df      Sum of Sq   RSS    AIC
#<none>                 22816 1042.4
#- bmi12    1     282.1 23098 1043.2
#- vyska12  1     289.9 23106 1043.2
#- hmot12   1     328.3 23144 1043.6
#- sport    2    1205.5 24021 1049.8
#- X.nvk11  1   14929.0 37745 1152.2

# Tvorba optimálního modelu dle AIC z krokové regrese
model <- lm(X.nvk12 ~ sport + hmot12 + vyska12 + bmi12 + X.nvk11, data = Deti)

# Koeficient vitální kapacity plic 11letých dětí
summary(model)$coefficients["X.nvk11",]
# Estimate   Std. Error      t value     Pr(>|t|) 
# 6.393365e-01 5.390292e-02 1.186089e+01 2.702549e-25 

#=> pokud se zvýší vit. kapacita plic u dítěte v 11 letech o 1 jedotku a ostatní ukazatele (bmi, sport, vaha...) zůstanou stejné
#     ,tak dochazí ke zvýšení vit. kapacity plic ve 12. o 0.642

#...............................................................................

# Kontrola kvality modelu - toho optimálního
summary(model)
#Residual standard error: 10.29 on 212 degrees of freedom
#Multiple R-squared:  0.4874,	Adjusted R-squared:  0.473  
#F-statistic: 34.07 on 6 and 215 DF,  p-value: < 2.2e-16

# Model vysvětluje 48.74% variability dat
# Standartní chyba předpovědí závislé proměné X.vit11 je 10.29

#Hodnocení kvality modelu - Graficky
par(mfrow = c(2, 2))
plot(model)

# Výstup z grafické kontroly:
#1. graf - Data mají správnou formu - OK
#2. graf - Data mají normální rozdělení (na krajích jsou outliery, ale vypadá to OK) - OK
#3. graf - Rozptyl hodnot je téměř konstantní - OK
#4. graf - Vzdálenost residuí - zase se zde nacházejí nějaké outliery, ale ponechávám je - OK 

# Test normality residuí
shapiro.test(residuals(model))
# alfa - hladiná významnosti = 0.05
#H0 (Nulová hypotéza): Residua modelu jsou normálně distribuovaná.
#H1 (Alternativní hypotéza): Residua modelu nejsou normálně distribuovaná.

#p-value = 0.009412 => Residua modelu nejsou normálně distribuovaná
pohlavi_kat <- levels(pohlavi)
sport_kat <- levels(sport)

# Definice našeho subjektu pro predikci
predict_input <- data.frame(
  pohlavi = factor("m", levels = pohlavi_kat),
  sport = factor("atl", levels = sport_kat),
  hmot12 = 40,
  vyska12 = 160,
  bmi12 = 40 / (1.6^2),
  syst12 = mean(Deti$syst12, na.rm = TRUE),
  dias12 = mean(Deti$dias12, na.rm = TRUE),
  X.nvk11 = 100
)

#Predikce vitální kapacity plic ve 12 letech - dle našich podmínek (okolností) v predict
predicted_res <- predict(model, newdata = predict_input)
predicted_res
# => 102.3169 - Výsledek predikce

#-------------------------------------------------------------------------------

#2. Vytvořte dvouhodnotovou proměnnou, která bude nabývat hodnoty 1 pro děti, co mají ve 12 letech vitální kapacitu větší než 100%, a 0 jinak. 
  #Opět najděte optimální model ze stejných nezávisle proměnných jako výše. Porovnejte kvalitu modelu s a bez nezávisle proměnné vitální kapacita plic v 
  #jedenácti letech. Porovnejte šance na vitální kapacitu plic ve dvanácti letech větší než 100% u těch, co dělají atletiku a u těch, co dělají plavání.

# Převod dat dle zadání
vit12_bin <- ifelse(vit12 > 100, 1, 0)
vit12_bin

# Přetypování sportů
Deti$sport <- relevel(Deti$sport, ref = "lh")

# Tvorba modelu
# => vybral jsem si logistickou regresi
# bez vit11
model2 <- glm(vit12_bin ~ sport + hmot12 + vyska12 + bmi12, family = binomial, data = Deti)

# s vit11
model3 <- glm(vit12_bin ~ sport + hmot12 + vyska12 + bmi12 + X.nvk11, family = binomial, data = Deti)

summary(model2)
summary(model3)

#Porovnání AIC - kvalita modelu
AIC(model2,model3)

#           df      AIC
# model2    6   251.0280 -- bez vit11
# model3    7   208.8242 -- s vit11

#Model3, který zahrnuje vitální kapacitu v 11 letech (X.nvk11), má nižší AIC hodnotu ve srovnání s Modelem2, který tuto proměnnou nezahrnuje. 
# To naznačuje, že Model3 je lepší z hlediska pokrytí variability a poskytuje lepší vyváženost mezi přesností a složitostí modelu.

#----------------------------------------------------------------

library(fmsb)
NagelkerkeR2(model3)
#=> 0.4215019

# interpretace regresnich koeficientu
b <- coef(model3)

# Porování šancí vitální kapacity
atletika <- (exp(b["sportatl"]))
plavani <- (exp(b["sportplavani"]))
res <- plavani/atletika # převracená hodnota - (atletika/plavani)**(-1)
res 

# Plavci mají 3.887x vyšší šanci na vyšší vitální kapacitu plic ve srovnání s atlety

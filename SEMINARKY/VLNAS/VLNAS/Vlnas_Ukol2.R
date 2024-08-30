
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
#- bmi12    1     282.1 23098 1043.2 | [-1042.4] = +0.8
#- vyska12  1     289.9 23106 1043.2 | [-1042.4] = +0.8
#- hmot12   1     328.3 23144 1043.6 | [-1042.4] = +1.2
#- sport    2    1205.5 24021 1049.8 | [-1042.4] = +7.4
#- X.nvk11  1   14929.0 37745 1152.2 | [-1042.4] = +109.8

# AIC hodnota v řádku Df s 'none' představuje model, který obsahuje všechny proměnné -> AIC skore tohoto stavu nám poslouží
#   jako základ pro porovnání, při vyřazování jednotlivých proměných (jejich vliv na AIC skore)

# Ostatní řádky s příslušnou proměnnou ukazují stav, kdy jsou z modelu vyřazeny - proto při odečtu AIC[i] od AIC['none'] zjistíme její vliv k celkovému modelu

# 1. X.nvk11:
#    - Tato proměnná má největší význam pro model.
#    - Vyřazení této proměnné by zvýšilo AIC o 109.8, což je největší nárůst mezi všemi proměnnými.
#    - Po jejím vyřazení se celkové AIC skóre zvýší výrazně, takže je velmi důležitá.

# 2. sport:
#    - Tato proměnná je také důležitá pro model.
#    - Vyřazení této proměnné by zvýšilo AIC o 7.4.

# 3. hmot12:
#    - Tato proměnná má střední význam pro model.
#    - Vyřazení této proměnné by zvýšilo AIC o 1.2.
#    - Po jejím vyřazení se celkové AIC skóre zvýší, ale ne výrazně.

# 4. bmi12 a vyska12:
#    - Tyto proměnné mají nejmenší význam mezi uvedenými, ale stále přispívají k modelu.
#    - Vyřazení těchto proměnných by zvýšilo AIC o 0.8.


# Tvorba optimálního modelu dle AIC z krokové regrese
model <- lm(X.nvk12 ~ sport + hmot12 + vyska12 + bmi12 + X.nvk11, data = Deti)

# Koeficient vitální kapacity plic 11letých dětí
summary(model)$coefficients["X.nvk11",]
# Estimate   Std. Error      t value     Pr(>|t|) 
# 6.393365e-01 5.390292e-02 1.186089e+01 2.702549e-25 

#=> pokud se zvýší vit. kapacita plic u dítěte v 11 letech o 1 jedotku a ostatní ukazatele (bmi, sport, vaha...) zůstanou stejné
#     ,tak dochazí ke zvýšení vit. kapacity plic ve 12. o 0.639

# Výpis jedotlivých koeficientů
summary(model)$coefficients

# Odhadnutá regresní rovnice:
# Y = 193.714 + 1.370 * sportatl + 7.726 * sportplavani + 1.920 * hmot12 −1.029 * vyska12 −4.408 * bmi12 + 0.639 * X.nvk11
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

#SMAZAT!!
#model1 <- lm(X.nvk12 ~ pohlavi + sport + hmot12 + vyska12 + bmi12 + syst12 + dias12 + X.nvk11, data = Deti)
#step(lm(X.nvk12 ~ pohlavi + sport + hmot12 + vyska12 + bmi12 + syst12 + dias12 + X.nvk11, data = Deti))

# Tvorba modelu
# => vybral jsem si logistickou regresi
# bez vit11
model2 <- glm(vit12_bin ~ pohlavi + sport + hmot12 + vyska12 + bmi12 + syst12 + dias12, family = binomial, data = Deti)

# s vit11
model3 <- glm(vit12_bin ~ pohlavi + sport + hmot12 + vyska12 + bmi12 + syst12 + dias12 + X.nvk11, family = binomial, data = Deti)

summary(model2)
summary(model3)

# Hledání optimálního modelu logistické regrese
step(model2) #-> model bet vit11
#Step:  AIC=247.18
#vit12_bin ~ sport + hmot12 + bmi12 + syst12 + dias12

#Df Deviance    AIC
#<none>        233.18 247.18
#- syst12  1   236.71 248.71
#- dias12  1   238.54 250.54
#- bmi12   1   242.41 254.41
#- hmot12  1   244.06 256.06
#- sport   2   247.87 257.87

#Optimální model bez vitálí kapacity:
opt_2 = glm(vit12_bin ~ sport + hmot12 + bmi12 + syst12 + Deti$dias12)

step(model3)
#Step:  AIC=208.55
#vit12_bin ~ sport + hmot12 + vyska12 + bmi12 + syst12 + dias12 + 
#  X.nvk11

#Df Deviance    AIC
#<none>         190.55 208.55
#- syst12   1   193.38 209.38
#- vyska12  1   193.78 209.78
#- dias12   1   193.82 209.82
#- bmi12    1   194.25 210.25
#- hmot12   1   194.30 210.30
#- sport    2   199.09 213.09
#- X.nvk11  1   232.83 248.83


# Optimálí model s vit11
opt_3 = glm(vit12_bin ~ sport + hmot12 + vyska12 + bmi12 + syst12 + Deti$dias12 + Deti$X.nvk11)

#Porovnání AIC - kvalita modelu
AIC(opt_2,opt_3)

#df      AIC
#opt_2  8 262.9339 - bez vit11
#opt_3 10 222.6565 - s vit11

#opt_3 (model), který zahrnuje vitální kapacitu v 11 letech (X.nvk11), má nižší AIC hodnotu ve srovnání s Modelem2, který tuto proměnnou nezahrnuje. 
# To naznačuje, že model opt_3 je lepší z hlediska pokrytí variability a poskytuje lepší vyváženost mezi přesností a složitostí modelu.

#----------------------------------------------------------------

library(fmsb)

NagelkerkeR2(opt_2)
# $N = 222
# R2 = 0.1766979

# => Tento model vysvětluje přibližně 17.67 % variability závislé proměnné


NagelkerkeR2(opt_3)
#N = 222
#R2 = 0.3366243

# => Tento model vysvětluje přibližně 33.66 % variability závislé proměnné.
# To znamená, že model opt_3 vysvětluje více variability závislé proměnné než model opt_2.
#   => model má lepší schopnost predikovat závislou proměnnou

# Interpretace regresnich koeficientu
b <- coef(opt_3)
b

# (Intercept)     sportatl sportplavani       hmot12      vyska12        bmi12       syst12  Deti$dias12 Deti$X.nvk11 
#  3.306791388  0.113347460  0.237553261  0.055390409 -0.026575673 -0.147464968  0.005799460 -0.006559751  0.013992883 

# Regresní rovnice pro omptimální model
# Y =  3.306791388 + 0.113347460 * sportatl + 0.237553261 * sportplavani + 
#      0.055390409 * hmot12 - 0.026575673 * vyska12 - 0.147464968 * bmi12 + 
#      0.005799460 * syst12 - 0.006559751 * dias12 + 0.013992883 * X.nvk11

# Porování šancí vitální kapacity
atletika <- (exp(b["sportatl"]))
plavani <- (exp(b["sportplavani"]))
res <- plavani/atletika # převracená hodnota - (atletika/plavani)**(-1)
res 

# Plavci mají v porovnání s atlety přibližně 1.132x vyšší šanci na vitální kapacitu plic přes 100%.

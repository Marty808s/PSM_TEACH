#Načtení dat
load("Deti.RData")
#Zobrazení prvních řádků dat
head(Deti)
#------------------------------
#1. V datech Deti.RData najděte optimální model závislosti vitální kapacity plic dvanáctiletých dětí
#(proměnná X.nvk12 - měřená v procentech) na jejich pohlaví, na sportu, který dělají,
#na váze, výšce, BMI, systolickém a diastolickém tlaku ve dvanácti letech a na vitální kapacitě plic
#v jedenácti letech (proměnné pohlavi, sport, hmot12, vyska12, bmi12, syst12, diast12 a X.nvk11).
#Napište odhadnutý model, interpretujte jeden vybraný regresní koeficient (ne absolutní člen), 
#ohodnoťte kvalitu modelu, zkontrolujte splnění předpokladů. Odhadněte, jakou vitální kapacitu by měl
#mít ve dvanácti letech kluk, který dělá atletiku, váží 40 kg, měří 160 cm a v jedenácti letech měl
#vitální kapacitu 100%. Pokud bude potřeba, uvažujte u něj průměrný tlak.


#Vytvoření lineárního modelu
model <- lm(X.nvk12 ~ pohlavi + sport + hmot12 + vyska12 + bmi12 + syst12 + dias12 + X.nvk11, data = Deti)
#Zobrazení souhrnu modelu
summary(model)
names(coef(model))
#Interpretace koeficientu sport
coef_sport <- summary(model)$coefficients["sportplavani",]
coef_sport
#Z koeficientů jsem si vybral koeficient pro plavání, protože je statisticky významný. p = 0.0135
#  Estimate Std. Error    t value   Pr(>|t|) 
#  5.69193962 2.28621637 2.48967670 0.01355462 
#Tento koeficinet znamená, že průměrná vitální kapacita plic dětí, které mají za sport
#plavání je o 5.692 vyšší, než u dětí, které dělají atletiku (reference pro proměnnou sport)
coef_nvk <- summary(model)$coefficients["X.nvk11",]
coef_nvk
#další zajímavý koeficient je vitální kapacita plic u 11 letých dětí.
#každé zvýšení vitální kapacity plic o 1 jednotku je spojeno se zvýšením vitální
#kapacity plic ve 12 letech o 0.642, pokud zůstanou stejné ostatní proměnné jako bmi,..
#hodnota p je v tomto případě extrémně nízka -> 2e-16
# Estimate   Std. Error      t value     Pr(>|t|) 
# 6.417782e-01 5.410799e-02 1.186106e+01 3.187370e-25 

#Kvalita modelu
summary(model)
# Residual standard error: 10.29 on 212 degrees of freedom
# Multiple R-squared:  0.4955,	Adjusted R-squared:  0.4741 
# F-statistic: 23.14 on 9 and 212 DF,  p-value: < 2.2e-16
#Standardní průměrná chyba mezi skutečnými a předpovězenými hodnotami závislé proměnné (X.nvk11)
#je 10.29. Dále vidíme, že model vysvětluje 49.55% variability (R-squared), adjusted R-squared je o něco
#nižší, některé proměnné tedy nemusí výrazně přispívat k vysvětlení variability.

#Grafy modelu pro hodnocení kvality
par(mfrow = c(2, 2))
plot(model)
#Z 1. grafu je patrné, že data mají správnou formu. Druhý graf, který se zaměřuje na
#normální rozdělení je patrné, že mmáme přítomných několik outlierů, nicméně máme normální rozdělení.
#Třetí graf nám řekne, že rozptyl hodnot je téměř konstantní
#Z posledního grafu je čitelné, že některá pozorování mají vysokou vzdálenost,
#ale rozhodl jsem se je ponechat.

#Test normality reziduí
shapiro.test(residuals(model))
#p-value = 0.005595, to znamená, že máme normální rozdělení
#náhled na kategorické proměnné pro správnou deifnici predikce
levels(Deti$pohlavi)
levels(Deti$sport)
#Definování specifického případu - kluk, 12 let, 40kg, 160cm, sport = atletika, vitální kapacita v 11 = 100
predikce <- data.frame(
  pohlavi = factor("m", levels = levels(Deti$pohlavi)),
  sport = factor("atl", levels = levels(Deti$sport)),
  hmot12 = 40,
  vyska12 = 160,
  bmi12 = 40 / (1.6^2),
  syst12 = mean(Deti$syst12, na.rm = TRUE),
  dias12 = mean(Deti$dias12, na.rm = TRUE),
  X.nvk11 = 100
)

#Predikce vitální kapacity plic ve 12 letech
predicted_val <- predict(model, newdata = predikce)
predicted_val
#Výsledek predikce našeho příkladu 103.2481 
#------------------------------
#2. Vytvořte dvouhodnotovou proměnnou, která bude nabývat hodnoty 1 pro děti, co
#mají ve 12 letech vitální kapacitu větší než 100%, a 0 jinak. Opět najděte optimální
#model ze stejných nezávisle proměnných jako výše. Porovnejte kvalitu modelu s a bez 
#nezávisle proměnné vitální kapacita plic v jedenácti letech. Porovnejte šance na
#vitální kapacitu plic ve dvanácti letech větší než 100% u těch, co dělají
#atletiku a u těch, co dělají plavání.

#Vytvoříme si binární proměnnou pro hodnoty vitální kapacity plic, kde při převýšení hodnoty 100 dáme 1, jinak 0
kapacita12_binarni <- ifelse(Deti$X.nvk12 > 100, 1, 0)
#Zvolil jsem logistickou regresi, jelikož máme binární proměnnou,
#prvně vytvoříme model bez proměnné X.nvk11
#Přetypování referenčního sportu, kvůli následné analýze
Deti$sport <- relevel(Deti$sport, ref = "lh")
model_without_X.nvk11 <- glm(kapacita12_binarni ~ pohlavi + sport + hmot12 + vyska12 + bmi12 + syst12 + dias12,
                             data = Deti, family = binomial)
summary(model_without_X.nvk11)
#Logistická regrese s proměnnou X.nvk11
model_with_X.nvk11 <- glm(kapacita12_binarni ~ pohlavi + sport + hmot12 + vyska12 + bmi12 + syst12 + dias12 + X.nvk11,
                          data = Deti, family = binomial)
summary(model_with_X.nvk11)
#Porovnání kvality modelů
#Porovnání AIC
AIC(model_without_X.nvk11, model_with_X.nvk11)
#                       df      AIC
#model_without_X.nvk11  9 250.0785
#model_with_X.nvk11    10 209.4920
#Podle hodnocení AIC (Akaikeho kritérium) je lepší model s vitální kapacitou v 11 letech,
#poskytuje tedy lepší přizpůsobení datům a složitosti modelu.
plot(model_with_X.nvk11)
plot(model_without_X.nvk11)

#Porovnání šancí na vitální kapacitu plic větší než 100 % mezi atletikou a plaváním
levels(Deti$sport)
odds_ratio_atletika <- exp(coef(model_with_X.nvk11)["sportatl"])
odds_ratio_plavani <- exp(coef(model_with_X.nvk11)["sportplavani"])
odds_ratio_atletika / odds_ratio_plavani
#Z porovnání nám vychází, že dítě, které dělá atletiku má 27,46% šanci oproti dítěti,
#které má za sport plavání na vitální kapacitu s hodnotou vyšší než 100 ve 12 letech.

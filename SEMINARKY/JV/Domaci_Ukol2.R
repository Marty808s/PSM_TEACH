library(fmsb)
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
# Kroková regrese pro nalezení optimálního modelu
opt_model <- step(model)
#Start:  AIC=1044.87
#X.nvk12 ~ pohlavi + sport + hmot12 + vyska12 + bmi12 + syst12 + 
#  dias12 + X.nvk11
#Df Sum of Sq   RSS    AIC
#- dias12   1      88.7 22542 1043.7
#- pohlavi  1      92.4 22546 1043.8
#<none>                 22453 1044.9
#- bmi12    1     204.1 22657 1044.9
#- syst12   1     211.8 22665 1045.0
#- vyska12  1     231.2 22685 1045.1
#- hmot12   1     251.8 22705 1045.3
#- sport    2    1296.3 23750 1053.3
#- X.nvk11  1   14900.3 37354 1155.9

#Step:  AIC=1043.74
#X.nvk12 ~ pohlavi + sport + hmot12 + vyska12 + bmi12 + syst12 + 
#  X.nvk11
#Df Sum of Sq   RSS    AIC
#- pohlavi  1     104.2 22646 1042.8
#- syst12   1     138.3 22680 1043.1
#<none>                 22542 1043.7
#- bmi12    1     246.7 22789 1044.2
#- vyska12  1     263.0 22805 1044.3
#- hmot12   1     290.1 22832 1044.6
#- sport    2    1225.8 23768 1051.5
#- X.nvk11  1   15126.7 37669 1155.7

#Step:  AIC=1042.77
#X.nvk12 ~ sport + hmot12 + vyska12 + bmi12 + syst12 + X.nvk11
#Df Sum of Sq   RSS    AIC
#- syst12   1     169.4 22816 1042.4
#<none>                 22646 1042.8
#- bmi12    1     260.7 22907 1043.3
#- vyska12  1     278.9 22925 1043.5
#- hmot12   1     298.6 22945 1043.7
#- sport    2    1149.6 23796 1049.8
#- X.nvk11  1   15032.3 37679 1153.8
model <- lm(X.nvk12 ~ sport + hmot12 + vyska12 + bmi12 + X.nvk11, data = Deti)
#Step:  AIC=1042.42
#X.nvk12 ~ sport + hmot12 + vyska12 + bmi12 + X.nvk11
#Df Sum of Sq   RSS    AIC
#<none>                 22816 1042.4
#- bmi12    1     282.1 23098 1043.2
#- vyska12  1     289.9 23106 1043.2
#- hmot12   1     328.3 23144 1043.6
#- sport    2    1205.5 24021 1049.8
#- X.nvk11  1   14929.0 37745 1152.2

#Interpretace koeficientu sport
coef_sport <- summary(model)$coefficients["sportplavani",]
coef_sport
#Z koeficientů jsem si vybral koeficient pro plavání, protože je statisticky významný. p = 0.004214542
#  Estimate  Std. Error     t value    Pr(>|t|)
#  6.355377838 2.197175241 2.892522052 0.004214542 
#Tento koeficinet znamená, že průměrná vitální kapacita plic dětí, které mají za sport
#plavání je o 6.355 vyšší, než u dětí, které dělají atletiku (reference pro proměnnou sport),
#za předpokladu, že ostatní proměnné jsou v modelu konstantní.
coef_nvk <- summary(model)$coefficients["X.nvk11",]
coef_nvk
#další zajímavý koeficient je vitální kapacita plic u 11 letých dětí.
#každé zvýšení vitální kapacity plic o 1 jednotku je spojeno se zvýšením vitální
#kapacity plic ve 12 letech o 0.639, pokud zůstanou stejné ostatní proměnné jako bmi,..
#hodnota p je v tomto případě extrémně nízka -> 2e-16
# Estimate   Std. Error      t value     Pr(>|t|)
# 6.393365e-01 5.390292e-02 1.186089e+01 2.702549e-25

#Kvalita modelu
summary(model)
# Residual standard error: 10.3 on 215 degrees of freedom
# Multiple R-squared:  0.4874,	Adjusted R-squared:  0.473  
# F-statistic: 34.07 on 6 and 215 DF,  p-value: < 2.2e-16
#Standardní průměrná chyba mezi skutečnými a předpovězenými hodnotami závislé proměnné (X.nvk11)
#je 10.3. Dále vidíme, že model vysvětluje 48.74% variability (R-squared), adjusted R-squared je o něco
#nižší, některé proměnné tedy nemusí výrazně přispívat k vysvětlení variability.

summary(model)$coefficients
# Odhadnutá regresní rovnice:
# Y = 195.084 - 1.370 * sportlh + 6.355 * sportplavani + 1.920 * hmot12 - 1.028 * vyska12 - 4.408 * bmi12 + 0.639 * X.nvk11

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
#p-value = 0.009412, to znamená, že memáme normální rozdělení
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
#Výsledek predikce našeho příkladu 102.3169 
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
#Vytvoříme maximální model pro jednotlivé scénaře, tedy s a bez proměnné X.nvk11
Deti$sport <- relevel(Deti$sport, ref = "lh")
model_without_X.nvk11 <- glm(kapacita12_binarni ~ sport + hmot12 + vyska12 + bmi12 + syst12 + dias12,
                             data = Deti, family = binomial)
summary(model_without_X.nvk11)

#Hledání optimálního modelu
step(model_without_X.nvk11)
#Start:  AIC=248.83
#kapacita12_binarni ~ sport + hmot12 + vyska12 + bmi12 + syst12 + dias12
#       Df   Deviance   AIC
#- vyska12  1   233.18 247.18
#- hmot12   1   234.03 248.03
#- bmi12    1   234.51 248.51
#<none>         232.83 248.83
#- syst12   1   236.24 250.24
#- dias12   1   237.95 251.95
#- sport    2   247.53 259.53

#Step:  AIC=247.18
#kapacita12_binarni ~ sport + hmot12 + bmi12 + syst12 + dias12
#       Df   Deviance  AIC
#<none>        233.18 247.18
#- syst12  1   236.71 248.71
#- dias12  1   238.54 250.54
#- bmi12   1   242.41 254.41
#- hmot12  1   244.06 256.06
#- sport   2   247.87 257.87

#Optimální model tedy eliminuje proměnnou vyska12 s výsledným AIC = 247.18
opt_model_without_X.nvk11 <- glm(kapacita12_binarni ~ sport + hmot12 + bmi12 + syst12 + dias12,
                             data = Deti, family = binomial)

summary(opt_model_without_X.nvk11)$coefficients
# Odhadnutá regresní rovnice:
# Y = 2.585 + 0.828 * sportatl + 2.327 * sportplavani + 0.142 * hmot12 - 0.495 * bmi12 + 0.034 * syst12 - 0.047 * dias12


#Logistická regrese s proměnnou X.nvk11
model_with_X.nvk11 <- glm(kapacita12_binarni ~ sport + hmot12 + vyska12 + bmi12 + syst12 + dias12 + X.nvk11,
                          data = Deti, family = binomial)
summary(model_with_X.nvk11)

#Hledáme optimální model s proměnnou X.nvk11
step(model_with_X.nvk11)
#Start:  AIC=208.55
#kapacita12_binarni ~ sport + hmot12 + vyska12 + bmi12 + syst12 + dias12 + X.nvk11
#         Df Deviance    AIC
#<none>         190.55 208.55
#- syst12   1   193.38 209.38
#- vyska12  1   193.78 209.78
#- dias12   1   193.82 209.82
#- bmi12    1   194.25 210.25
#- hmot12   1   194.30 210.30
#- sport    2   199.09 213.09
#- X.nvk11  1   232.83 248.83

#V tomto případě je maximální model optimální 

#Odhadnutá regresní rovnice:
summary(model_with_X.nvk11)$coefficients
# Y = 40.74402665 + 0.676 *sportatl + 2.091 * sportplavani + 0.635 * hmot12 - 0.327 * vyska12 - 1.575 * bmi12 + 0.034 * syst12 - 0.042 * dias12 + 0.099 * X.nvk11

#Porovnání kvality modelů
#Porovnání AIC
AIC(opt_model_without_X.nvk11, model_with_X.nvk11)
#                           df    AIC
#opt_model_without_X.nvk11  7 247.1812
#model_with_X.nvk11         9 208.5477
#Podle hodnocení AIC (Akaikeho kritérium) je lepší model s vitální kapacitou v 11 letech,
#poskytuje tedy lepší přizpůsobení datům a složitosti modelu.
NagelkerkeR2(model_with_X.nvk11)
NagelkerkeR2(opt_model_without_X.nvk11)
#Podle NAgelkerkeR2 vysvětluje model s vitální kapacitou v 11 letech 44.03% variability,
#bez vitální kapacity v 11 letech vysvětluje 23.47%. Model s vitální kapcitou v 11 letech je přesnější
plot(model_with_X.nvk11)
plot(opt_model_without_X.nvk11)

#Porovnání šancí na vitální kapacitu plic větší než 100 % mezi atletikou a plaváním
levels(Deti$sport)
odds_ratio_atletika <- exp(coef(model_with_X.nvk11)["sportatl"])
odds_ratio_plavani <- exp(coef(model_with_X.nvk11)["sportplavani"])
odds_ratio_plavani / odds_ratio_atletika
#Z porovnání nám vychází, že dítě, které dělá plavání má 4.118888 krát větší šanci oproti dítěti,
#které má za sport atletiku na vitální kapacitu s hodnotou vyšší než 100 ve 12 letech.

#------------------------------
#DOPLNĚNÍ:
#Při porušení normality bych měl přemýšlet o vhodné transformaci závislých proměnných (např. logaritmickou,
#případně použití druhé odmocniny závislé proměnné), případně přetypovat model pomocí glm funkce.
#Při porušení stability rozptylu (homoskedasticity) můžeme transformovat nezávislé proměnné, případně využít metodu
#vážených nejmenších čtverců.

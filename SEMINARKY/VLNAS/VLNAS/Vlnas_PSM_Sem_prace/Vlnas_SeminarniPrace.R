# Ředitel banky si u Vás objednal analýzu výsledků dotazníkového šetření realizovaného na skupině zákazníků banky. Jeho dotaz zní: jak bych měl navýšit zisk banky?

#i) Mezi marketéry velkých firem se klade zásadní důraz na následující tři proměnné: celková spokojenost, ochota firmu doporučit, víra že u firmy vydržím. 
# Můžete se zaměřit na některou z těchto proměnných, nebo na jejich kombinaci (např. pomocí váženého průměru) a hledat regresní optimální model,
#na čem tato proměnná závisí. Následně pak vyhodnotíte, na které faktory by se banka měla zaměřit především.

# Manipulace s daty
data <- satisfaction
data

# Definice proměnných pro každý sloupec
reputation <- data$reputation
trustworthiness <- data$trustworthiness
seriousness <- data$seriousness
solidness <- data$solidness
care <- data$care
exp_products <- data$exp_products
exp_services <- data$exp_services
service <- data$service
solutions <- data$solutions
quality <- data$quality
qual_products <- data$qual_products
qual_services <- data$qual_services
range_products <- data$range_products
qual_personal <- data$qual_personal
qual_overall <- data$qual_overall
benefits <- data$benefits
investments <- data$investments
price <- data$price
satisfaction <- data$satisfaction
expectations <- data$expectations
comparison <- data$comparison
performance <- data$performance
return <- data$return
switch <- data$switch
recommendation <- data$recommendation
loyalty <- data$loyalty
gender <- data$gender

#-------------------------------------------------------------------------------

# Vztah satisfaction/recommendation/loyalty - NEMÁ PŘÍNOS NA CELKOVÉ ŠETŘENÍ (iformativní)
summary(satisfaction)
summary(recommendation)
summary(loyalty)

model <- lm(satisfaction ~ recommendation + loyalty, data = data)

# Optimální regresní model
optimal_model <- step(model)

#Step:  AIC=225.09
#satisfaction ~ recommendation

summary(optimal_model)
# Multiple R-squared:  0.4047 - Tento koeficient určuje, že model vysvětluje přibližně 40,47% variability závislé proměnné (spokojenost)

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)     3.89923    0.27130   14.37   <2e-16 ***
#  recommendation  0.48247    0.03716   12.98   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# => Doporučení - každá jednotka zvýšená v ochotě doporučit zvyšuje spokojenost o 0.48247 jedotek
# => Udržitelnost - ta z optimálního modelu kvůli AIC vypadla


# Model s loyalty - kvůli lepší představě
summary(lm(satisfaction ~ recommendation + loyalty, data = data))

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)     3.67282    0.36183  10.151   <2e-16 ***
#  recommendation  0.46338    0.04229  10.958   <2e-16 ***
# loyalty         0.04687    0.04955   0.946    0.345   

# => Doporučení - každá jednotka zvýšená v ochotě doporučit zvyšuje spokojenost o 0.46338 jedotek
# => Loyalita (důvěra) - každá jednotka zvýšená v důvěře zvyšuje spokojenost o 0.04687  jednotek
# ===> to znamená, že Doporučení je pro firmu důležitější, jelikož má násobně větší vliv, proto není součástí optimálího modelu (viz. nahoře)

#-------------------------------------------------------------------------------

# Hledání oprimálního modelu - nezávislé proměnné satisfaction/recommendation/loyalty
model_satisfaction <- lm(satisfaction ~ reputation + trustworthiness + seriousness + solidness + care +
                           exp_products + exp_services + service + solutions + quality +
                           qual_products + qual_services + range_products + qual_personal + qual_overall +
                           benefits + investments + price, data = data)

model_recommendation <- lm(recommendation ~ reputation + trustworthiness + seriousness + solidness + care +
                             exp_products + exp_services + service + solutions + quality +
                             qual_products + qual_services + range_products + qual_personal + qual_overall +
                             benefits + investments + price, data = data)

model_loyalty <- lm(loyalty ~ reputation + trustworthiness + seriousness + solidness + care +
                      exp_products + exp_services + service + solutions + quality +
                      qual_products + qual_services + range_products + qual_personal + qual_overall +
                      benefits + investments + price, data = data)

#DOPSAT SLOVNÍ VÝSTUPY.....
# Optimálí model pro celkovou spokojenot - vůči všem proměnným
sat <- step(model_satisfaction)
#Step:  AIC=79.22
#satisfaction ~ seriousness + exp_products + service + range_products +  
  #benefits + investments + price
summary(sat)

rec <- step(model_recommendation)
#Step:  AIC=337.36
#recommendation ~ trustworthiness + solidness + exp_services + 
#  qual_products + qual_personal + benefits + investments + 
#  price
summary(rec)

loy <- step(model_loyalty)
#Step:  AIC=327.06
#loyalty ~ solidness + exp_products + service + quality + qual_products + 
#  range_products + qual_personal + investments
summary(loy)
#-------------------------------------------------------------------------------

# Hlavní komponenty
data_without_gender <- data[, !names(data) %in% 'gender']
data_scaled <- scale(data_without_gender)
cor <- cor(data_scaled)
res_eigen <- eigen(cor)
res_eigen

# Vizuální náhled hl. komponent pomocí screeplotu
screeplot(princomp(data_scaled), type="l")
abline(h=1, col="red")
# => z grafu lze poznat, že první 5 komponenty budou hlavní! - nachází se nad hladinou v grafu a na hladině

# Výpočet variability - pro lepší identifikaci, které komponenty jsou hlavní
cumsum(res_eigen$values/sum(res_eigen$values))
# 0.4742973 0.5544377 0.6013454 0.6456726 0.6831606
# => prvních 5 komponent obsáhnou 68,3% variability dat - proto jsou tedy hlavní komponenty

# Faktorová analýza
res_fact <- factanal(data_scaled, factors = 5)
res_fact$loadings

#Factor1 Factor2 Factor3 Factor4 Factor5
#reputation               0.148   0.689   0.187   0.113 
#trustworthiness  0.319   0.295   0.766   0.182         
#seriousness      0.291   0.414   0.678   0.176         
#solidness                0.143   0.479   0.264   0.110 
#care             0.428   0.304   0.299   0.185   0.167 
#exp_products     0.488   0.177                   0.844 
#exp_services     0.751   0.236   0.155           0.125 
#service          0.625   0.177   0.128           0.142 
#solutions        0.522   0.194   0.207   0.248   0.290 
#quality          0.666   0.191   0.189           0.205 
#qual_products    0.531   0.258           0.282   0.492 
#qual_services    0.748   0.318   0.136   0.232         
#range_products   0.646   0.288                   0.107 
#qual_personal    0.574   0.244   0.269   0.424         
#qual_overall     0.683   0.262   0.168   0.228         
#benefits         0.514   0.570   0.234   0.188   0.111 
#investments      0.269   0.546   0.201   0.328   0.146 
#quality.1        0.252   0.392   0.244   0.314   0.146 
#price            0.453   0.494   0.283   0.279         
#satisfaction     0.322   0.774   0.242   0.237   0.169 
#expectations     0.438   0.736   0.210   0.171         
#comparison       0.291   0.634   0.193   0.144         
#performance      0.236   0.580   0.286   0.264         
#return           0.228   0.467   0.228   0.577   0.129 
#switch           0.152   0.186   0.235   0.520         
#recommendation   0.289   0.444   0.221   0.551         
#loyalty                  0.136   0.170   0.720         

#Factor1 Factor2 Factor3 Factor4 Factor5
#SS loadings      5.555   4.330   2.740   2.578   1.326
#Proportion Var   0.206   0.160   0.101   0.095   0.049
#Cumulative Var   0.206   0.366   0.468   0.563   0.612

# Faktory:
#1. Kvalita služeb a produktů - exp_service, service, qual_service, qual_overall, quality.....
#2. Zákaznícká spokojennost a očekávání - satisfaction, expectations, benefits, performance, comparsion....
#3. Důvěra - trustworthiness, seriousness, solidnes....
#4. Loyalita - loyality, return, switch, recommendation....
#5. Málo významný (kvalita produktu - čistě / 'Uspokojení potřeby') - exp_product, qual_product....

# Banka by se měla určitě zaměřit na první faktor, který má pro ní největší vliv

#------------------------------------------------------------------------------

#ii) Využijete stejnou myšlenku jako v bodě i), ale nebudete pracovat se všemi hodnotami závisle proměnné, ale uděláte si jen dvouhodnotovou proměnnou
#(např. odchod od banky: ano x ne) a budete hledat rizikové faktory pro odchod klientů od banky pomocí logistické regrese.

# Vytvoříme si proměnnou status - 1-neodejde 0-odejde (předpokladem bude spokojenost záíkazíka - satisfaction )
summary(data$satisfaction)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.00    6.00    7.00    7.18    8.00   10.00 

summary(data$qual_overall)

# Z popisných statistik bych šel cestou -> pokud zákazníková spokojenost je < 5 nebo kvalita celkového procesu < 5
# Pokud nebudu ákupem uspokojen, nebo kvalita jak produktu, nebo celkového procesu byla spíš přítěž, tak se rozhodně nevrátím :).
sum(data$satisfaction<5)
sum(data$qual_overall<5)
data[data$satisfaction < 5 | data$qual_overall < 5 ,]

# Binární proměnná status
data$status <- ifelse(data$satisfaction < 4 | data$qual_overall < 3, 0, 1)
status <- data$status

model_status = glm(status ~ reputation + trustworthiness + seriousness + solidness + care +
                     exp_products + exp_services + service + solutions + quality +
                     qual_products + qual_services + range_products + qual_personal + qual_overall +
                     benefits + investments + price, data = data, family = binomial())

# Optimální model
step(model_status)

#Step:  AIC=86.51
#status ~ exp_products + exp_services + service + qual_services + 
#  qual_personal + qual_overall + benefits + investments + price

status_model_optimal <- glm(status ~ exp_products + exp_services + service + qual_services + 
                            qual_personal + qual_overall + benefits + investments + price, data = data, family = binomial())

summary(status_model_optimal)

#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#  (Intercept)    -5.8668     1.7545  -3.344 0.000826 ***
#  exp_products    0.2713     0.1660   1.635 0.102137    
#  exp_services    0.5069     0.2804   1.808 0.070647 .  
#  service        -0.2899     0.1904  -1.523 0.127737    
#  qual_services  -0.5672     0.3183  -1.782 0.074750 .  
#  qual_personal  -0.4679     0.2827  -1.655 0.097855 .  
#  qual_overall    0.6094     0.2207   2.761 0.005764 ** 
#  benefits        0.4346     0.2182   1.991 0.046446 *  
#  investments     0.4204     0.1888   2.226 0.025993 *  
#  price           0.5656     0.2171   2.605 0.009185 ** 


# Model naznačuje, že qual_overall, benefits, investments, a price jsou významnými prediktory pro status. 
# Proměnné exp_services, qual_services, a qual_personal jsou na hranici statistické významnosti, což by mohlo znamenat, 
# že by mohly mít vliv.

#------------------------------------------------------------------------------

#Načtení potřebných knihoven
library(tidyverse)
library(dplyr)
library(MASS)

#Načtení dat ze souboru
load("SatData.RData")

#Přejmenování datového rámce pro lepší přehlednost
Sat <- satisfaction
colnames(Sat) <- make.unique(colnames(Sat))
#Přidání nových sloupců k datovému rámci
Sat <- Sat %>%
  mutate(
    nespokojen_a_chce_odejit = ifelse(satisfaction < 5 & loyalty < 5, 1, 0),
    spokojen_nechce_odejit_nedoporuci = ifelse(satisfaction >= 5 & loyalty >= 5 & recommendation < 5, 1, 0),
    spokojen_nechce_odejit_doporucuje = ifelse(satisfaction >= 5 & loyalty >= 5 & recommendation >= 5, 1, 0),
    chce_odejit_ale_je_spokojen = ifelse(satisfaction >= 5 & loyalty < 5, 1, 0)
  )

#Kontrola nových sloupců
table(Sat$nespokojen_a_chce_odejit)
table(Sat$spokojen_nechce_odejit_nedoporuci)
table(Sat$spokojen_nechce_odejit_doporucuje)
table(Sat$chce_odejit_ale_je_spokojen)

#Výběr proměnných pro diskriminační analýzu
selected_vars <- c("reputation", "trustworthiness", "seriousness", "solidness","care",
                   "exp_products", "exp_services", "service", "solutions", "quality",
                   "qual_products", "qual_services", "range_products", "qual_personal", "qual_overall",
                   "benefits", "investments", "price", "gender")

all_vars <- c(selected_vars, "nespokojen_a_chce_odejit", 
              "spokojen_nechce_odejit_nedoporuci", 
              "spokojen_nechce_odejit_doporucuje", "chce_odejit_ale_je_spokojen")

#Filtrace dat na základě vybraných proměnných a skupiny
Sat_filtered <- Sat %>%
  dplyr::select(all_of(all_vars)) %>%
  filter(complete.cases(.))

#Provádění diskriminační analýzy pro každou binární proměnnou zvlášť
lda_model_nespokojen <- lda(nespokojen_a_chce_odejit ~ ., data = Sat_filtered)
lda_model_nechce_nedoporuci <- lda(spokojen_nechce_odejit_nedoporuci ~ ., data = Sat_filtered)
lda_model_nechce_doporucuje <- lda(spokojen_nechce_odejit_doporucuje ~ ., data = Sat_filtered)
lda_model_chce_spokojen <- lda(chce_odejit_ale_je_spokojen ~ ., data = Sat_filtered)

#Výsledky diskriminační analýzy
print(lda_model_nespokojen)
print(lda_model_nechce_nedoporuci)
print(lda_model_nechce_doporucuje)
print(lda_model_chce_spokojen)

#Predikce skupin pro každou binární proměnnou
predictions_nespokojen <- predict(lda_model_nespokojen)
predictions_nechce_nedoporuci <- predict(lda_model_nechce_nedoporuci)
predictions_nechce_doporucuje <- predict(lda_model_nechce_doporucuje)
predictions_chce_spokojen <- predict(lda_model_chce_spokojen)

#Přidání predikovaných skupin do datového rámce
Sat_filtered <- Sat_filtered %>%
  mutate(predicted_nespokojen = predictions_nespokojen$class,
         predicted_nechce_nedoporuci = predictions_nechce_nedoporuci$class,
         predicted_nechce_doporucuje = predictions_nechce_doporucuje$class,
         predicted_chce_spokojen = predictions_chce_spokojen$class)

#Kontingenční tabulky pro skutečné vs. predikované skupiny
table(Sat_filtered$nespokojen_a_chce_odejit, Sat_filtered$predicted_nespokojen)
table(Sat_filtered$spokojen_nechce_odejit_nedoporuci, Sat_filtered$predicted_nechce_nedoporuci)
table(Sat_filtered$spokojen_nechce_odejit_doporucuje, Sat_filtered$predicted_nechce_doporucuje)
table(Sat_filtered$chce_odejit_ale_je_spokojen, Sat_filtered$predicted_chce_spokojen)

library(ggplot2)

#Definování barvy pro jednotlivé skupiny
Sat_filtered <- Sat_filtered %>%
  mutate(group = case_when(
    nespokojen_a_chce_odejit == 1 ~ "Nespokojený a chce odejít",
    spokojen_nechce_odejit_nedoporuci == 1 ~ "Spokojený, nechce odejít, ale nedoporučuje",
    spokojen_nechce_odejit_doporucuje == 1 ~ "Spokojený, nechce odejít a doporučuje",
    chce_odejit_ale_je_spokojen == 1 ~ "Chce odejít, ale je spokojený",
    TRUE ~ "Ostatní"
  ))

#Vytvoření grafu
ggplot(Sat_filtered, aes(x = trustworthiness, y = exp_services, color = group)) +
  geom_point(alpha = 0.9) +
  labs(title = "Diskriminační Analýza: Hodnocení Důvěryhodnosti a Zkušeností",
       x = "Důvěryhodnost (trustworthiness)",
       y = "Zkušenosti s poskytovanými službami (exp_services)",
       color = "Skupina") +
  theme_minimal()

# Predikce pomocí LDA modelů
pred_nespokojen <- predict(lda_model_nespokojen)$class
pred_nechce_nedoporuci <- predict(lda_model_nechce_nedoporuci)$class
pred_nechce_doporucuje <- predict(lda_model_nechce_doporucuje)$class
pred_chce_spokojen <- predict(lda_model_chce_spokojen)$class

# Skutečné hodnoty
actual_nespokojen <- Sat_filtered$nespokojen_a_chce_odejit
actual_nechce_nedoporuci <- Sat_filtered$spokojen_nechce_odejit_nedoporuci
actual_nechce_doporucuje <- Sat_filtered$spokojen_nechce_odejit_doporucuje
actual_chce_spokojen <- Sat_filtered$chce_odejit_ale_je_spokojen

# Výpočet přesnosti
accuracy_nespokojen <- mean(pred_nespokojen == actual_nespokojen)
accuracy_nechce_nedoporuci <- mean(pred_nechce_nedoporuci == actual_nechce_nedoporuci)
accuracy_nechce_doporucuje <- mean(pred_nechce_doporucuje == actual_nechce_doporucuje)
accuracy_chce_spokojen <- mean(pred_chce_spokojen == actual_chce_spokojen)

# Výpis výsledků
accuracy_nespokojen
accuracy_nechce_nedoporuci
accuracy_nechce_doporucuje
accuracy_chce_spokojen

# Při použití hlavních proměnných je přesnost modelů menší, nicméně stále velmi vysoká.
# accuracy_nespokojen
# 0.936
# accuracy_nechce_nedoporuci
# 0.936
# accuracy_nechce_doporucuje
# 0.936
# accuracy_chce_spokojen
# 0.936

# Při použití všech proměnných je o 2.8% vyšší.
# accuracy_nespokojen
# 0.972
# accuracy_nechce_nedoporuci
# 0.964
# accuracy_nechce_doporucuje
# 0.964
# accuracy_chce_spokojen
# 0.956

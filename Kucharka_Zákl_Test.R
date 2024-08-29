##########################
#KUCHAŘKA
##########################

# zÁKLADNÍ TESTY:
# Když zkoukám zda může být: průměrná hodnota v jedné skupině:
# TEST NORMALITY DAT:
#   1. mají normální rozdělení -> t.test 
#   2. nemají -> wilcox.test()


# Když zkoukám zda může být: liší se hodnota rozdílu ve dvou NEzávislých skupinách (CISLO X CISLO):
# TEST NORMALITY DAT: 
#   1. mají normální rozdělení -> TEST SHODY ROZPTYLU -> Mají shodné rozptyly - ttest
#   2.                                                -> nemají - Welchův test
#   3. nemají normální rozdělení -> dvouvýběrový Wilcoxonův test 


# Když zkoukám zda může být: liší se hodnota rozdílu ve dvou ZÁvislých skupinách (CISLO X CISLO):
# 1. TEST NORMALITY DAT:   
#   mají normální rozdělení -> párový t.test
#   nemají -> wilcox.test()


# Když zkoukám zda může být: liší se hodnota X (průměr hodnoty X) ve více závislých skupinách (NEŽ 2):
# 1. TEST NORMALITY DAT:   
#   mají normální rozdělení -> ANOVA
#   nemají -> Friedman


# Když zkoukám zda může být: liší se hodnota X (průměr hodnoty X) ve více NEzávislých skupinách (NEŽ 2):
# 1. TEST NORMALITY DAT:   
#   1. mají normální rozdělení -> TEST SHODY ROZPTYLU -> Mají shodné rozptyly - ANOVA pro shodné rozptyly
#   2.                                                -> nemají - ANOVA pro různé rozptyly
#   3. nemají normální rozdělení -> Kruskal-Wallis



##########################

#Chi-Kvadrát - porování (KATEGORIE X KATEGORIE)
##########################

# Souvisi spolu......

#1. Chci-kvadrát test - více jak 5 pozorování pro 1 kategorii
#2. Fisherův test -> pokud nesplním bod 1, tak použiji Fisherův test...


##########################

# Věcná významnost
# Je vyznamny rozdil ve ___ mezi _____ a _____?

# Podle použitého testu viz. výše použiji metodu pro získání vvěcné významnosti

##########################

#Korelace
#=> pro dvě číselé hodnoty
#Souvisí spolu dvě číselné proměnné?

##########################

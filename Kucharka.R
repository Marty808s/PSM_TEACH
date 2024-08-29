##########################
#KUCHAŘKA
##########################

# zÁKLADNÍ TESTY:
# Když zkoukám zda může být: průměrná hodnota v jedné skupině:
# TEST NORMALITY DAT:
#   1. mají normální rozdělení -> t.test 
#   2. nemají -> wilcox.test()


# Když zkoukám zda může být: liší se průměrná hodnota ve dvou NEzávislých skupinách (CISLO X CISLO):
# 1. TEST NORMALITY DAT:   
#   mají normální rozdělení -> t.test -> jdu kouknout na rozptyly
#   nemají -> wilcox.test() -> kouknu na rozptyly, ale nemám jinou volbu (podmínkou jsou..)
# 2. TEST SHODOSTI ROZPTYLŮ
#   jsou shodne -> t.test
#   nejsou shodne -> welchuv test


# Když zkoukám zda může být: liší se průměrná hodnota ve dvou ZÁvislých skupinách (CISLO X CISLO):
# 1. TEST NORMALITY DAT:   
#   mají normální rozdělení -> párový t.test -> jdu kouknout na rozptyly
#   nemají -> wilcox.test()


# Když zkoukám zda může být: průměrná hodnota ve dvou nezávislých skupinách:
# TEST NORMALITY RESIDUI LIN. MODELU: 
#   1. mají normální rozdělení -> ANOVA var.equal = TRUE - default....
#   2. nemají -> ANOVA var.equal = FALSE
# TEST SHODY ROZPTYLU:
#   1. maji shodny rozptyl ->
#   2. nemaji shodny rozptyl -> KRUSKAL ANOVA
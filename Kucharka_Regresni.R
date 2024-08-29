#### Regresní modely

# Lineární model

#1. test předpokladá - plot(model)
# 1. Residuals - nesmí mít trend
# 2. Má být na přímce - normalita
# 3. Stabilita rozptylu - nema mit trend
# 4. Vlivnost pozorování - zadny bod nesmi prekrocovat mez (vzdálenost)

# ==> summary(model) => R2

# Logistická regrese
# -> glm(, family="binomial")
# -> PODMÍNKA: závislá proměná je Boolean hodnota

# ===>NagelkeR2 = R2 pro log. reg
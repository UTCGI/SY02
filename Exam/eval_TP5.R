load("cctp_P2023.RData")


# La variable à régresser se situe à gauche, le ou les régresseurs à droite.
donnees = RL0305

# Régression des données vary en fonction des données varx
m <- lm(vary~varx, data = donnees)
m <- lm(Y~X, data = RL05)
m
# Trouver â = intercept
a <- m$coefficients[1]
# Trouver b^ = varx
b <- m$coefficients[2]

summary(m)

# variance totale
(SY2 <- mean((RL02$Y - mean(RL02$Y))^2))

# variance expliquée par le régression
(Sreg <- mean((m$fitted.values - mean(RL02$Y))^2))

# variance résiduelle
(Sres <- mean(m$residuals^2))

R2 <- Sreg/SY2
R2
## 10eme residu
m$residuals[10]


# ---- Intervalle de confiance ----
# Donnez la borne inférieur de l'intervalle de confiance bilatéral au point 0.1 au niveau 95%
predict(m, data.frame(X = 0.1), interval = "confidence", level = 0.95)

# IC sur les coefficients de la droite des moindres carrés au niveau de confiance 1 − α = 0.99.
# Borne inf de l'IC bilatéral au niveau 99% pour la pente b -> prendre la valeur en X
# Si c'est pour a, prendre intercept
confint(m, level = 0.99)




# ----------  Prediction en 1 point ------------
# Quel est la prédiction au premier élément de X ?
# Prédisez la valeur de Y pour le premier élément de X :
predict(m, data.frame(X = RL02$X[1]))

# Prédiction au point 0
predict(m, data.frame(X = 0), interval="prediction")

##prediction au point X : fit (95% de base)
predict(reg, data.frame(X=XXXX), interval="prediction")




##borne inf d'une int de confiance sur la pente b au niveau 95%
confint(m, 'X', 0.95)




# La variable à régresser se situe à gauche, le ou les régresseurs à droite.
donnees = RL0305

# Régression des données vary en fonction des données varx
m <- lm(vary~varx, data = donnees)
# Trouver â = intercept
a <- m$coefficients[1]
# Trouver b^ = varx
b <- m$coefficients[2]

# variance totale
(SY2 <- mean((donnees$vary - mean(donnees$vary))^2))

# variance expliquée par le régression
(Sreg <- mean((m$fitted.values - mean(donnees$vary))^2))

# variance résiduelle
(Sres <- mean(m$residuals^2))

R2 <- Sreg/SY2


# IC sur les coefficients de la droite des moindres carrés au niveau de confiance 1 − α = 0.99.
confint(m, level = 0.99)

# IC Sur les prédictions des valeurs au pts 97 et 100
newdata <- data.frame(Temp = c(97,100))
predict(m, newdata, interval = "confidence")

## 10eme residu

m$residuals[10]


newdata = RL05

reg = lm(Y ~ X, data = newdata)

##infos sur a et b
reg

##infos R?
summary(reg)

##Intervalle de confiance au point X au niveau XXXXXX : borne inf = lwr
predict(reg, data.frame(X=XXXX), interval="confidence", level = 0.95)

predict(reg, data.frame(X=0.1), interval="confidence", level = 0.95)

##prediction au point X : fit (95% de base)
predict(reg, data.frame(X=XXXX), interval="prediction")

##borne inf d'une int de confiance sur la pente b au niveau 95%
confint(reg, 'X', 0.95)

# Intervalle de confiance sur les coefficients de la droite des moindres carrés au niveau de confiance 1 − α = 0.99.
confint(rl, level = 0.99)


install.packages("jsonlite", repos = 'https://cran.r-project.org')

# Questions :
# 1. Ordre des variables dans la fonction lm
# 2. Explication de la fonction summary - confint - predict
# 3. différence entre confint et predict



# réaliser la régression des données vary en fonction des données varx
donnees <- data.frame(varx = c(0, 0.2, 0.3, 0.6),
                        vary = c(1.01, 1.44, 1.55, 2.1))
# La variable à régresser se situe à gauche, le ou les régresseurs à droite.
lm(vary~varx, data = donnees)

plot(donnees$varx, donnees$vary)
m <- lm(vary ~ varx, data = donnees)
a <- m$coefficients[1]
b <- m$coefficients[2]
b
abline(a, b)

sum(m$residuals)

(a + b * mean(donnees$varx))

mean(donnees$vary)

# variance totale
(SY2 <- mean((donnees$vary - mean(donnees$vary))^2))

# variance expliquée par le régression
(Sreg <- mean((m$fitted.values - mean(donnees$vary))^2))

# variance résiduelle
(Sres <- mean(m$residuals^2))

# Variance totale (= SY2)
Sres + Sreg

R2 <- Sreg/SY2
R2

summary(m)

# Coefficient de corrélation de Pearson
cor(donnees$varx, donnees$vary, method = "pearson")^2
cor(donnees$vary, m$fitted.values, method = "pearson")^2


attach(anscombe)

anscombe$x1
x1

summary(lm(y1 ~ x1))
# Prédiction
# 10 ème résidu 
# Prédiction en un point
# IC
# Réalisation de a chapeau, b chapeau

hooker <- read.csv("data/hooker-data.data")
plot(hooker$Temp, hooker$Pression)
rl <- lm(Pression ~ Temp, data = hooker)
summary(rl)$r.squared
qqnorm(rl$residuals)
qqline(rl$residuals)
plot(rl$fitted.values, rl$residuals)

# IC sur les coefficients de la droite des moindres carrés au niveau de confiance 1 − α = 0.99.
confint(rl, level = 0.99)
# TODO : A quoi correspondent les valeurs affichés ?

# Valeur de la prédiction au point 97 et au point 100.
# Fit correspond à la vrai prédiction, lwr et upr sont les bornes sup et inf de l'intervalle de confiance pr la prédiction
newdata <- data.frame(Temp = c(97,100))
predict(rl, newdata, interval = "confidence")



moore <- read.csv("data/moore-data.data")
rl.moore <- lm(log(Transistor.count) ~ Date.of.introduction, data = moore)
summary(rl.moore)

(IC <- confint(rl.moore, "Date.of.introduction"))
exp(predict(rl.moore, newdata = data.frame(Date.of.introduction = c(2018)), interval ="confidence"))

exp(predict(rl.moore, newdata = data.frame(Date.of.introduction = c(2018)), interval ="prediction"))

log(2)/IC


cedar <- read.csv('data/cedar-data.data')
rl.cedar = lm(height ~ diameter, data = cedar)
summary(rl.cedar)
rl.cedar$coefficients[1]
rl.cedar$coefficients[2]


boxcox <- function(x, lambda) {
if (lambda == 0)
log(x) else (x^lambda - 1)/lambda
}


for (lambda in c(-1, -1/2, 0, 1/3, 1/2, 1)) {
plot(cedar$diameter, cedar$height, main = paste("lambda = ", lambda))
cedar$logdiameter <- boxcox(cedar$diameter, lambda)
rl.cedar <- lm(height ~ logdiameter, data = cedar)
print(summary(rl.cedar)$r.squared)
curve(rl.cedar$coefficients[1] + rl.cedar$coefficients[2] * boxcox(x, lambda), add = TRUE)
}


cedar <- read.csv("data/cedar-data.data")
rl.cedar <- lm(height ~ log(diameter), data = cedar)
summary(rl.cedar)

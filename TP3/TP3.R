# générer un échantillon de taille n :
  
runifa <- function(n) {
    if(!exists("param")) 
      param <<- sample(10:20, 1)
    runif(n, min = 0, max = param)
}

# Q1 Calcul de l'estimateur de a sachant que E(X) = a/2
estim <- function(echantillon){
  a <- 2*mean(echantillon)
  return (a)
}

estim(runifa(100))

# Q2
n <- 100
a <- replicate(1000, estim(runifa(n)))
boxplot(a)
param

# Q3

estim_k <- function(echantillon , ordre){
  ek <- echantillon ^ ordre
  a <- ((ordre + 1) * mean(ek)) ^ (1 / ordre) 
  return (a)
}

n <- 100
a <- replicate(1000, estim_k(runifa(n), 5))
boxplot(a)
param

# Théorème central limite
runknown <- function(n) {
  bn <- rbinom(n, 1, 0.2)
  bn * rnorm(n, mean=-4, sd=1) + (1 - bn) * rnorm(n, mean=10, sd=1)
}


n <- 1000
x <- runknown(n)
mean(x)
sd(x)
sd(x)^2

hist(x)

plot(ecdf(x))


mu <- 7.2
sigma = sqrt(32.36)

T <- (mean(x) - mu) / (sigma / sqrt(n))
T


random.T <- function(n) {
  # Générer le vecteur x de taille n
  x <- runknown(n)
  # Calculer une réalisation de la loi T
  T <- (mean(x) - mu) / (sigma / sqrt(n))
  return(T)
}

t.1000 <- replicate(1000, random.T(n))
t.1000
mean(t.1000)
var(t.1000)
sd(t.1000)


plot(ecdf(t.1000))
curve(pnorm, add=TRUE)


f <- function(lambda, x){
  res <- lambda * exp(-lambda * x)
  res [x < 0] <- 0
  return (res)
}

f2 <- function(lambda, x) {
  dexp(x, rate = lambda)
}


L <- function(lambda, x){
  # Produit de toutes les valeurs prises par al fonction f
  # Par définition de la fonction de vraisemblance
  prod(f(lambda, x))
}

logL <- function(lambda, x) {
  sum(log(f2(lambda, x)))
}


# Q17 -> Il faut prendre la plus grande valeur entre ce que renvoie logL(2.8) et logL(3.1)
# Car pour la vraisemblance, on cherche a trouver le maximum de cette fonction
x <- rexp(100, rate = 3)
x
logL(2.8,x)
logL(3.1,x)

lambdas <- seq(0, 6, 0.01)
logL.lambdas <- sapply(lambdas, function(lambda) logL(lambda, x))
plot(lambdas, logL.lambdas, type = "l")
# calcule la log–vraisemblance des lambdas et affiche le graphe correspondant. Le maximum de cette
# fonction de vraisemblance est atteint vers λ = 3.

# calcul du vrai maximum
x <- rexp(n, rate = 3)
opt <- optimize(logL, lower = 0, upper = 6, maximum = TRUE, x = x)
opt$maximum

sim.EMV <- function(){
  x <- rexp(n, rate = 3)
  opt <- optimize(logL, lower = 0, upper = 6, maximum = TRUE, x = x)
  opt$maximum
}

sim.EMV()

sim.EMV.10000 <- replicate(10000, sim.EMV())
sim.EMV.10000

# Espérance de lambda chapeau
mean(sim.EMV.10000)
boxplot(sim.EMV.10000)
# Var(lambda chapeau)
var(sim.EMV.10000)

# Estimation du biais
# => Biais = E(lambda) - valeur théorique de lambda
mean(sim.EMV.10000) - 3

((10000 * 3)/ (10000 - 1)) -3



library(pracma)

sim.Fisher <- function() {
  x <- rexp(n, rate = 3)
  # Log-vraisemblance par rapport à x généré précédement
  logLx <-function(lambda) {logL(lambda, x)} # On obtient une fonction qui peut être dérivée 
  
  # Information de Fisher
  (grad(logLx, 3))^2
}

n<-1000

(info.Fisher <- mean(replicate(n, sim.Fisher())))
info.Fisher


n / (3^2)

# Par le thm 5.3 de la page 68, pour n assez grand, 
# lambda chapeau ~ N(lambda, 1/In(lambda)) 

1 / info.Fisher

var(sim.EMV.10000)


grad2 <- function(fct, val){
  deriv1 <- function(val){
    grad(fct, val)
  }
  grad(deriv1,val)
}

sim.Fisher <- function() {
  x <- rexp(n, 3)
  logLx <- function(lambda) logL(lambda, x)
  grad2(logLx, 3)
}
(-mean(replicate(1000, sim.Fisher())))


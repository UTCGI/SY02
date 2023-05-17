library(MASS) # Charge la bibliothèque MASS

head(painters)
# Q1
painters$Composition
hist(painters$Composition)
hist(painters$Drawing)
hist(painters$Colour)
hist(painters$Expression)
hist(painters$School)


# moyenne <- mean(sum(painters$Composition , painters$Drawing , painters$Colour , painters$Expression , painters$School))


# Q2
# vecteur moyenne la moyenne des quatre notes de chaque peintre
painters_2 <- painters
moyenne <- (painters$Composition + painters$Drawing + painters$Colour + painters$Expression) / 4

moyenne

painters_2$Moyenne <- moyenne
head(painters_2)

painters_2 <- subset(painters_2, select = -c(Composition:School))

head(painters_2)


# Q3


n <- length(moyenne)

MoyenneEmpirique <- sum(moyenne) / n
MoyenneEmpirique

VarianceEmpirique <- (sum(moyenne^2) / n) - MoyenneEmpirique^2
VarianceEmpirique

EquartTypeEmpirique <- sqrt(VarianceEmpirique)

EquartTypeEmpirique

VarianceEmpiriqueCorrigé <- (n * VarianceEmpirique) / (n-1)
VarianceEmpiriqueCorrigé

EquartTypeEmpiriqueCorrigé <- sqrt(VarianceEmpiriqueCorrigé)
EquartTypeEmpiriqueCorrigé

 # Q4
mean(moyenne) #Moyenne empirique
var(moyenne) # VarianceEmpiriqueCorrigé
sd(moyenne) # EquartTypeEmpiriqueCorrigé

var(moyenne) * ((n - 1)/n) # VarianceEmpirique

sd(moyenne) * sqrt(((n - 1)/n) ) # EquartTypeEmpirique


# Q5
hist(moyenne)


# Partie 2

# On fait 1 - P(X < 3) pour obtenir P(X>=3)
1 - pnorm(3) # P(X >= 3) avec X qui suit une loi normale

# une variable normale d’espérance 35 et d’écart-type 6 est inférieure à 42 
pnorm(mean=35, sd=6, 42)# P(X <= 42)


# une variable normale d’espérance 35 et d’écart-type 6 est comprise entre 40 et 50 ;
# P(40 < X < 50)
pnorm(mean = 35, sd=6, 50) - pnorm(mean = 35, sd=6, 40)  

# obtenir n − 1 faces sur n lancers d’une pièce de monnaie équilibrée, avec n = 5, 10, 30 
n <- c(5,10,30)
dbinom(n-1,n, 0.5)

# obtenir strictement plus de 14 faces sur 20 lancers d’une pièce de monnaie équilibrée 
1 - pbinom(14,20,0.5)


# dbinom(10,20,0.5) -> P(X = 10)
# pbinom(10,20,0.5) -> P(X < 10)
# P(10 < X < 15)
sum(dbinom(10:15,20,0.5))

# Q7

alpha <- c(0.05, 0.1, 0.9)

# fractiles d’ordre α = 0.05, 0.1, 0.9 Loi normale centrée réduite
qnorm(alpha)

# fractiles d’ordre α = 0.05, 0.1, 0.9 loi du χ2 à 10 degrés de liberté 
qchisq(alpha,10)

# fractiles d’ordre α = 0.05, 0.1, 0.9 loi de Student à 5 degrés de liberté 
qt(alpha, 5)

# fractiles d’ordre α = 0.05, 0.1, 0.9 loi de Fisher à 2 et 5 degrés de liberté
qf(alpha, 2, 5)


# Partie 3

puissance <- function(a, b) {
  if (any(a < 0) & b > -1 & b < 1)
    stop("non définie pour a < 0 et -1 < b < 1")
  if (any(a == 0) & b < 0)
    stop("non définie pour a = 0 et b < 0")
  return(a^b)
}



dloi <- function(x, b) {
  if (b <= 0)
    stop("on doit avoir b > 0")
  a <- 2/b^2
  f <- a * x
  f[x < 0] <- 0
  f[x > b] <- 0
  return(f)
}

dloi(-1:5,3)

curve(dloi(x,3), from = -5, to = 5)

# Fonction de répartition : P(X <= t) = F(X) = Integrale entre -inf et t de f(x) -> fonction de densité
# ET on sait que F(0) = 0 et F(inf) = 1
ploi <- function(x,b) {
  if (b <= 0)
    stop("on doit avoir b > 0")
  f <- x^2 / b^2
  f[x>b] <- 1
  f[x<0] <- 0
  return(f)
}

curve(ploi(x, 3), from = -5, to = 5)

# Q13 : Isoler le x de la question précédente

qloi <- function(alpha, b){
  if (b <= 0)
    stop("on doit avoir b > 0")
  if (any(alpha < 0) | any(alpha > 1))
    stop("on doit avoir 0 <= alpha <= 1")
  f <- b * sqrt(alpha)
  f[alpha == 0] <- 0
  f[alpha == 1] <- b
  return(f)
}

curve(qloi(x, 3), from = 0, to = 1)

rloi <- function(n,b){
  if (b <= 0)
    stop("on doit avoir b > 0")
  if (!is.integer(n) | n < 1)
    stop("n doit être un entier strictement positif")
  u <- runif(n)
  x <-qloi(u,b)
  return(x)
}

rloi(5,3)

par(mfrow = c(2, 2))
for (n in c(10, 50, 100, 1000)) {
  hist(rloi(n, 3), breaks = round(1 + 10/3 * log10(n)), freq = FALSE, main = n, xlim = c(-1, 4))
  curve(dloi(x, 3), add = TRUE, col = "red")
}

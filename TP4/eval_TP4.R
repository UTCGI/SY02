load("cctp_P2023.RData")

# IC a 90% : 0.90 = 1 - a => a = 1 - 0.90 = 0.10 donc alpha = 0.10
# IC sur mu avec sigma carré connu 

x <- c(161, 155, 142, 157, 150, 158, 156, 165 ,163, 170)

IC_mu_sigma_connu <- function(x, alpha, sigma) {
  n <- length(x)
  mean(x) + c(-1, 1) * qnorm(1 - alpha/2) * sigma/sqrt(n)
}
IC_mu_sigma_connu(x, 0.1, sqrt(40))


# IC sur mu avec sigma carré inconnu
IC_mu_sigma_inconnu <- function(x, alpha) {
  n <- length(x)
  mean(x) + c(-1, 1) * qt(1 - alpha/2, df = n - 1) * sd(x)/sqrt(n)
}
IC_mu_sigma_inconnu(x, 0.1)

# IC sur sigma carré avec mu connu
IC_sigma_mu_connu <- function(x, alpha, mu) {
  n <- length(x)
  a <- sum((x - mu)^2)/(qchisq(1 - alpha/2, df = n ))
  b <- sum((x - mu)^2)/(qchisq(alpha/2, df = n ))
  return (c(a, b))
}
IC_sigma_mu_connu(x, 0.01, mean(x))


IC_sigma_mu_inconnu <- function(x, alpha) {
  n <- length(x)
  a <- (n - 1) * var(x)/(qchisq(1 - alpha/2, df = n - 1))
  b <- (n - 1) * var(x)/(qchisq(alpha/2, df = n - 1))
  return (c(a, b))
}
# IC_sigma_mu_inconnu(rnorm(100, mean = 3), 0.05)


# Sigma = Variance
# Mu = Moyenne

# IC sur la moyenne

# IC sur la moyenne avec la variance inconnu
IC_mu_sigma_inconnu(IC232, 0.01)
# IC sur la moyenne avec la variance connu
IC_mu_sigma_connu(IC, alpha, sigma)

# IC sur la variance

# IC sur la variance avec la moyenne inconnu
IC_sigma_mu_inconnu(IC232, 0.01)
# IC sur la variance avec la moyenne connu
IC_sigma_mu_connu(IC, alpha , mu)


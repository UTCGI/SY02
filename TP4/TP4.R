
n <- 100
mu <- 3
sigma <- 2
chisq1 <- function() {
  x <- rnorm(n, mean = mu, sd = sigma)
  (n - 1) * sd(x)^2/(sigma^2)
}
chisq1000 <- replicate(1000, chisq1())
hist(chisq1000, freq = FALSE)


hist(chisq1000, freq = FALSE)
curve(dchisq(x, df = n - 1), add = TRUE)


n <- 100
mu <- 3
sigma <- 2
stu1 <- function() {
  x <- rnorm(n, mean = mu, sd = sigma)
  (mean(x) - mu)/(sd(x)/sqrt(n))
}

stu1000 <- replicate(1000, stu1())
hist(stu1000, freq = FALSE)
curve(dt(x, df = n - 1), add = TRUE)




n <- 100
sigma <- 3
x <- rnorm(n, mean = 42, sd = sigma)
alpha <- 0.05
mean(x) + c(-1, 1) * qnorm(1 - alpha/2) * sigma/sqrt(n)


# Q4

n <- 100
alpha <- 0.05
x <- rnorm(n)
mean(x) + c(-1, 1) * qt(1 - alpha/2, df = n - 1) * sd(x)/sqrt(n)
# [1] -0.1459436 0.2189071
t.test(x, conf.level = 1 - alpha)$conf.int



# Q5
gen_IC <- function(x, alpha) {
  n <- length(x)
  mean(x) + c(-1, 1) * qt(1 - alpha/2, df = n - 1) * sd(x)/sqrt(n)
}
gen_IC(rnorm(100, mean = 3), 0.05)

# Q6
param <- 3
alpha <- 0.05
ICs <- replicate(100, gen_IC(rnorm(100, mean = param), alpha))
ICs

# Q7
source("./utils.R")
plot_ICs(ICs, param)
# Il y a 95% de traits verts


# Q8

alpha <- 0.05
ICs10 <- replicate(100, gen_IC(rnorm(10, mean = param), alpha))
ICs100 <- replicate(100, gen_IC(rnorm(100, mean = param), alpha))
ICs1000 <- replicate(100, gen_IC(rnorm(1000, mean = param), alpha))
plot_ICs(ICs10, param, xlim = c(1.5, 4.5), main = "n = 10")
plot_ICs(ICs100, param, xlim = c(1.5, 4.5), main = "n = 100")
plot_ICs(ICs1000, param, xlim = c(1.5, 4.5), main = "n = 1000")

# Q9

alpha <- 0.05
ICs10 <- replicate(100, gen_IC(rnorm(100, mean = param, sd = 1), alpha))
ICs100 <- replicate(100, gen_IC(rnorm(100, mean = param, sd = 2), alpha))
ICs1000 <- replicate(100, gen_IC(rnorm(100, mean = param, sd = 4), alpha))
plot_ICs(ICs10, param, xlim = c(1, 5), main = "sigma = 1")
plot_ICs(ICs100, param, xlim = c(1, 5), main = "sigma = 2")
plot_ICs(ICs1000, param, xlim = c(1, 5), main = "sigma = 4")

# Q10
# Taux de recouvrement
hit <- function(n, param, alpha) {
  x <- rnorm(n, mean = param)
  IC <- gen_IC(x, alpha)
  param >= IC[1] & param <= IC[2]
}
n <- 100
alpha <- 0.05
hm <- replicate(10000, hit(n, 3, alpha))
mean(hm)

# Q11
slutsky <- function(p, n, k, alpha) {
  sim <- function() {
    x <- rbinom(n, 1, p)
    phat <- mean(x)
    IC <- phat + c(-1, 1) * qnorm(1 - alpha/2) * sqrt(phat * (1 - phat)/n)
    p >= IC[1] & p <= IC[2]
  }
  mean(replicate(k, sim()))
}
slutsky(0.02, 100, 10000, 0.05)
# Q12
p <- 0.02
k <- 10000
alpha <- 0.05
ns <- floor(10^seq(1, 4, length.out = 30))
slpt <- sapply(ns, function(n) slutsky(p, n, k, alpha))
plot(log10(ns), slpt, type = "l", col = "red")


# Q13
noslutsky <- function(p, n, k, alpha) {
  sim <- function() {
    x <- rbinom(n, 1, p)
    phat <- mean(x)
    u <- qnorm(1 - alpha/2)
    IC <- (2 * n * phat + u^2 + c(-1, 1) * u * sqrt(u^2 + 4 * n * phat * (1 - phat)))/(2 * n + 2, * u^2)
    p >= IC[1] & p <= IC[2]
  }
  mean(replicate(k, sim()))
}
nslpt <- sapply(ns, function(n) noslutsky(p, n, k, alpha))
plot(log10(ns), slpt, type = "l", col = "red")
lines(log10(ns), nslpt, col = "green")
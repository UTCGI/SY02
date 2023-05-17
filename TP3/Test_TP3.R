load("cctp_P2023.RData")

#pois # loi de Poisson -> discret
logLpois <- function(lambda, x) {
  sum(log(dpois(x, lambda)))
}

logLpois(11,EMV439)

#exp # loi exponentielle -> continue
logLexp <- function(lambda, x) {
  sum(log(dexp(x, lambda)))
}
logLexp(0.75,EMV243)
logLexp(1.5,EMV243)
logLexp(2,EMV243)


#chisq # loi du X2 -> continue
logLchisq <- function(liberte, x) {
  sum(log(dchisq(x, df=liberte)))
}
logLchisq(12, EMV345)
logLchisq(16, EMV345)
logLchisq(28, EMV345)


#binom # loi binomiale -> discret
logLbinom <- function(proba_succes, x,nb_essaies) {
  sum(log(dbinom(x,nb_essaies, proba_succes)))
}


#unif # loi uniforme -> continue
logLunif <- function( x,min, max) {
  sum(log(dunif(x,min, max)))
}


#norm # loi normale -> continue
logLnorm <- function( x,moyenne, equart_type) {
  sum(log(dnorm(x,moyenne, equart_type)))
}


#t # loi de Student -> continue
logLstudent <- function( x,liberte) {
  sum(log(dt(x,liberte)))
}


#f # loi de Fisher -> continue
logLfisher <- function( x,liberte1, liberte2) {
  sum(log(df(x,liberte1,liberte2)))
}

#####################
### Vraisemblance ###
#####################

#pois # loi de Poisson -> discret
Lpois <- function(lambda, x) {
  prod(dpois(x, lambda))
}

Lpois(11,EMV439)

#exp # loi exponentielle -> continue
Lexp <- function(lambda, x) {
  prod(dexp(x, lambda))
}
Lexp(0.75,EMV243)
Lexp(1.5,EMV243)
Lexp(2,EMV243)


#chisq # loi du X2 -> continue
Lchisq <- function(liberte, x) {
  prod(dchisq(x, df=liberte))
}
Lchisq(12, EMV345)
Lchisq(16, EMV345)
Lchisq(28, EMV345)


#binom # loi binomiale -> discret
Lbinom <- function(proba_succes, x,nb_essaies) {
  prod(dbinom(x,nb_essaies, proba_succes))
}


#unif # loi uniforme -> continue
Lunif <- function( x,min, max) {
  prod(dunif(x,min, max))
}


#norm # loi normale -> continue
Lnorm <- function( x,moyenne, equart_type) {
  prod(dnorm(x,moyenne, equart_type))
}


#t # loi de Student -> continue
Lstudent <- function( x,liberte) {
  prod(dt(x,liberte))
}


#f # loi de Fisher -> continue
Lfisher <- function( x,liberte1, liberte2) {
  prod(df(x,liberte1,liberte2))
}


logLpois(11,EMV439)
logLpois(14,EMV439)
logLpois(17.5,EMV439)


logLchisq(12, EMV328)
logLchisq(18, EMV328)
logLchisq(24, EMV328)
logLchisq(12,EMV345)
logLchisq(16,EMV345)
logLchisq(28,EMV345)


logLexp(0.75,EMV243)
logLexp(1.5,EMV243)
logLexp(2,EMV243)


logLexp(0.5,EMV221)
logLexp(1.5,EMV221)
logLexp(2,EMV221)

logLchisq(10,EMV346)
logLchisq(20,EMV346)
logLchisq(22,EMV346)


logLpois(12,EMV401)
logLpois(13,EMV401)
logLpois(17.5,EMV401)


logLnorm(EMV132, 2, sqrt(0.5))
logLnorm(EMV132, 3, sqrt(0.5))
logLnorm(EMV132, 5, sqrt(0.5))

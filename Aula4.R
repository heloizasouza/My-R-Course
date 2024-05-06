rm(list = ls())

# 18° Distribuições de Probabilidade Discreta

# Poisson

# gerando valores aleatórios de uma Poisson
x <- rpois(n = 1000, lambda = 7)

# gerando valores da densidade da Poisson
y <- dpois(x = x, lambda = mean(x))

plot(x, y, main = "Poisson")


# Binomial

# gerando valores aleatórios de uma Binomial

x <- rbinom(n = 1000, size = 450, prob = 1/7)

# gerando valores da densidade de uma Binomial
y <- dbinom(x = x, size = 450, prob = 1/7)

plot(x, y, main = "Binomial")


# 19° Distribuições de Probabilidade Continuas

# Normal

# gerando valores aleatórios de uma Poisson
x <- rnorm(n = 1000)

# gerando valores da densidade da Poisson
y <- dnorm(x = x)

# Plot da densidade da Normal para um x de valores normais
plot(x, y, main = "Normal")

# Definindo um novo domínio para a densidade
x <- seq(from=0,to=6,length.out=100)
ylim<-c(0,0.6)

# Plot da densidade da Normal para um x qualquer
plot(x,dnorm(x,mean=3,sd=1), main="Normal", type="l", ylim=ylim) 
lines(x, dnorm(x,mean=5,sd=1), col = "red")
lines(x, dnorm(x,mean=2,sd=0.8), col = "blue")


# Uniforme

# Definindo um novo domínio para a densidade
x <- seq(from=0,to=6,length.out=100)
ylim<-c(0,0.6)

# Plot da densidade da Uniform para um x qualquer
plot(x, y = dunif(x,min=2,max=4), main="Uniforme", type="l", ylim=ylim)
lines(x, dunif(x,min=1,max=3), col = "blue")


# Exponencial

# gerando valores aleatórios de uma Exponencial
x <- rexp(n = 1000, rate = 1/2)

# gerando valores da densidade da Exponencial
y <- dexp(x,rate=1/2)

# Plot da densidade da Exponencial para um x de valores exp
plot(x, y, main = "Exponencial")

# Definindo um novo domínio para a densidade
x <- seq(from=0,to=6,length.out=100)
ylim<-c(0,0.6)

# Plot da densidade da Exponencial para um x qualquer
plot(x, y = dexp(x,rate=1/2),main="Exponential",type="l",ylim=ylim)
lines(x, dexp(x,rate=1/4), col = "red")
lines(x, dexp(x,rate=2), col = "blue")


# Gamma

# gerando valores aleatórios de uma Gamma
x <- rgamma(n = 1000, shape = 2)

# gerando valores da densidade da Gamma
y <- dgamma(x,shape=2,rate=1)

# Plot da densidade da Gamma para um x de valores gamma
plot(x, y, main = "Gamma")

# Definindo um novo domínio para a densidade
x <- seq(from=0,to=6,length.out=100)
ylim<-c(0,0.6)

# Plot da densidade da Gamma para um x qualquer
plot(x,y = dgamma(x,shape=2,rate=1),main="Gamma",type="l",ylim=ylim)
lines(x, dgamma(x,shape=4,rate=1), col = "red")
lines(x, dgamma(x,shape=1,rate=2), col = "blue")


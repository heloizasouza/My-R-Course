rm(list = ls())


# 18° Distribuições de Probabilidade Discreta -----------------------------



# Poisson

# gerando valores aleatórios de uma Poisson
x <- rpois(n = 1000, lambda = 7)

# gerando valores do quantile da distribuição
qpois(p = 0.975, lambda = 10)
qpois(p = 0.975, lambda = 4)

# densidade de probabilidade e distribuição acumulada
x <- seq(0,20,1)
par(mfrow=c(1,2))
plot(x, dpois(x, lambda = 4), type='o',pch=20,main="f.d.p. para X~Poisson")
points(x, dpois(x,lambda = 10),col="red",type='o',pch=20)
plot(x, ppois(x,lambda=4),type='o',pch=20,main="f.d.a. para X~Poisson")
points(x, ppois(x,lambda=10),col="red",type='o',pch=20)



# Binomial

# gerando valores aleatórios de uma Binomial
x <- rbinom(n = 1000, size = 450, prob = 1/7)

# gerando valores do quantile da distribuição
qbinom(p = 0.975, size = 20, prob = 1/4)
qbinom(p = 0.975, size = 20, prob = 1/9)

# densidade de probabilidade e distribuição acumulada
x <- seq(0,20,1)
par(mfrow=c(1,2))
plot(x, dbinom(x, size = 10, prob = 0.5), type='o',pch=20,main="f.d.p. para X~Binom")
points(x, dbinom(x, size=15,prob=0.8),col="red",type='o',pch=20)
plot(x, pbinom(x, size = 100, prob = 1/21), type='o',pch=20,main="f.d.p. para X~Binom")
points(x, pbinom(x, size=15,prob=0.8),col="red",type='o',pch=20)



# 19° Distribuições de Probabilidade Continuas ----------------------------


# Normal

# gerando valores aleatórios de uma Normal
x <- rnorm(n = 1000)

# gerando valores do quantile da distribuição - os valores tabelados
qnorm(p = 0.975)
qnorm(p = 0.05)

# densidade de probabilidade e distribuição acumulada
x<-seq(-3,3,0.1)
par(mfrow=c(1,2))
curve(dnorm(x = x, mean = 0, sd=1), xlim=c(-3,3), ylim=c(0,0.6), 
      col="black",lwd=2, main="f.d.p. da Distrib. Normal padrão")
lines(x=c(0,0), y=c(0,dnorm(0)), lty=6, col="gray")
lines(x, dnorm(x,mean=-2,sd=1), col = "red")
curve(pnorm(q = x, mean = 0, sd=1), xlim=c(-3,3), 
      col="black",lwd=2, main="f.d.a. da Distrib. Normal padrão")
lines(x=c(0,0), y=c(0,pnorm(0)), lty=6, col="gray")
lines(x, pnorm(x,mean=-2,sd=1), col = "red")



# Exponencial

# gerando valores aleatórios de uma Exponencial
x <- rexp(n = 1000, rate = 1/2)

# gerando valores do quantile da distribuição
qexp(p = c(0.05,0.025,0.95,0.975))

# densidade de probabilidade e distribuição acumulada
x <- seq(0,5,0.01)
par(mfrow=c(1,2))
curve(dexp(x,rate=1),xlim=c(0,5),main="f.d.p para X~exponencial")
lines(x, dexp(x,rate=1/2), col = "red")
curve(pexp(x,1),xlim=c(0,5),main="f.d.a para X~exponencial")
lines(x, pexp(x,rate = 1/2), col = "red")



# Gamma

# gerando valores aleatórios de uma Gamma
x <- rgamma(n = 1000, shape = 2)

# gerando valores do quantile da distribuição
qgamma(p = c(0.05,0.01,0.99,0.95), shape = 1)

# densidade de probabilidade e distribuição acumulada
x <- seq(0,10,0.01)
curve(dgamma(x,shape = 3, scale = 1),xlim=c(0,10),main="f.d.p. para X~Gama")
lines(x, dgamma(x,shape=4,rate=1), col = "red")
curve(pgamma(x,shape = 3, scale = 1),xlim=c(0,10),main="f.d.a. para X~Gama")
lines(x, pgamma(x,shape=4,rate=1), col = "red")



# Qui-quadrado.

# gerando valores aleatórios de uma Gamma
x <- rchisq(n = 1000, df = 2)

# gerando valores do quantile da distribuição
qchisq(p = c(0.05,0.01,0.99,0.95), df = 2)
qchisq(p = c(0.05,0.01,0.99,0.95), df = 10)

# densidade de probabilidade e distribuição acumulada
par(mfrow=c(1,2))
x <- seq(0,20,0.01)
curve(dchisq(x,df=2),xlim=c(0,20),ylim=c(0,0.5),
      main="f.d.p. para X~Qui-Quadrado")
lines(x, dchisq(x,df=4),col="red",xlim=c(0,20),ylim=c(0,0.5))
curve(pchisq(x, df=2),xlim=c(0,10),ylim=c(0,1),
      main="f.d.a. para X~Qui-Quadrado")
lines(x, pchisq(x,df=4),col="red",xlim=c(0,10),ylim=c(0,1))



# 20° Testes Estatísticos -------------------------------------------------


# TESTE DE ADERÊNCIA E ASSOCIAÇÃO


# Teste Qui-quadrado

# testa se a frequência observada difere da frequência esperada 
# sendo a fe especificada por uma distribuição de probabilidade
fo=c(0,6,8,11,7,4,3,1,0) #correspondentes a 0,1,2,3,4,5,6,7 e +7 falhas
fe1 =c(0,1,2,3,4,5,6,7)
fe=dpois(fe1, 3.2) #calculo da frequência esperada de 0 a 7 falhas
x=1-sum(fe) #calculo da frequência esperada de ‘mais de 7 falhas’
fe=c(fe,x) #adição de ‘mais de 7 falhas’ ao numero esperado de falhas
chisq.test(fo,p=fe)


# Teste Kolmogorov-Smirnof

# Exemplo 1 -- x tem a mesma distribuição de y? 
# Teste usando uma amostra e uma dist conhecida
x <- seq(from=0,to=6,length.out=100)
ks.test(x = x, y = "dt")


# Exemplo 2 -- x e y tem a mesma distribuição? 
# Teste usando duas amostras
x <- rnorm(50)
y <- runif(30)
ks.test(x, y)


# Teste Shapiro-Wilk de Normalidade
shapiro.test(x)



# TESTE DE HOMOGENEIDADE E INDEPENDÊNCIA


# Teste Exato de Fisher para tabelas 2x2

# Exemplo 1 -- a testadora acertou o chute do gosto do chá?
TeaTasting <-matrix(c(3, 1, 1, 3),nrow = 2,
         dimnames = list(Guess = c("Milk", "Tea"),
                         Truth = c("Milk", "Tea")))
fisher.test(TeaTasting, alternative = "greater")


# Exemplo 2 -- o tipo de divórcio difere pela classe?
divorcio <- matrix(c(3,2,2,3),nrow=2,
                   dimnames = list(Tipo = c("amigavel","nao amigavel"),
                                   Classe = c("alta","media")))
fisher.test(divorcio, alternative = "greater")


# Teste Qui-quadrado para tabelas de contingência

# Exemplo 1 -- a escolha do partido independe do sexo?
M <- as.table(rbind(c(762, 327, 468), c(484, 239, 477)))
dimnames(M) <- list(gender = c("F", "M"),
                    party = c("Democrat","Independent", "Republican"))
M
chisq.test(M)

# Exemplo 2 -- as notas nas disciplinas são independentes?
c_hum <- c(15,20,30,20,15)
c_bio <- c(8,23,18,34,17)
D <- as.table(rbind(c_hum,c_bio))
chisq.test(D)


rm(list = ls())

# carregando esse pacote para fazer transformações nos dados, fazer gráficos e 
# usar o conjunto de dados diamonds do pacote ggplot2
library(tidyverse)

# pacote usado para executar o comando HSD.test
library(agricolae)

# Abrindo a lista do Pacote "datasets" de conjuntos de dados disponíveis no R
library(help = "datasets")


# Carregamento de dados e Transformações

# carregamento de conjunto de dados

# conjunto de dados diamonds usado na regressão múltipla
data("diamonds")
# conjunto de dados mtcars usado na regressão múltipla
data("mtcars")


# transformando covariáveis numéricas em categóricas
mtcars <- mtcars |>
  mutate(cylf = as.factor(as.character(cyl)),
         gearf = as.factor(as.character(gear)),
         carbf = as.factor(as.character(carb)))

# transformando covariáveis numéricas em categóricas
diamonds <- diamonds |>
  mutate(cutf = as.factor(as.character(cut)),
         clarityf = as.factor(as.character(clarity)))


# 16° Regressão Linear Múltipla -------------------------------------------


# modelo de regressão múltipla de efeitos principais

mod1 <- lm(formula = mpg ~ hp + carbf + wt + cylf, data = mtcars)
# resumo dos resultados do ajuste do modelo
summary(mod1)

# gráficos de diagnóstico dos resíduos
plot(mod1)

# obtendo os resíduos do modelo anova ajustado
residuo <- residuals(mod1)
# teste de normalidade dos resíduos de Shapiro-Wilk
shapiro.test(residuo)

fit1 <- lm(formula = price ~ carat + depth + cutf, data = diamonds)
# resumo dos resultados do ajuste do modelo
summary(fit1)

# gráficos de diagnóstico dos resíduos
plot(fit1)

# obtendo os resíduos do modelo anova ajustado
residuo <- residuals(fit1)
# teste de normalidade dos resíduos de Shapiro-Wilk
shapiro.test(residuo)


# modelo de regressão múltipla com interações dupla

mod2 <- lm(formula = mpg ~ hp*wt + hp*cylf + wt*cylf, data = mtcars)
# resumo dos resultados do ajuste do modelo
summary(mod2)

# gráficos de diagnóstico dos resíduos
plot(mod2)

# obtendo os resíduos do modelo anova ajustado
residuo <- residuals(mod2)
# teste de normalidade dos resíduos de Shapiro-Wilk
shapiro.test(residuo)


fit2 <- lm(formula = price ~ carat*depth + carat*cutf + depth*cutf, data = diamonds)
# resumo dos resultados do ajuste do modelo
summary(fit2)

# gráficos de diagnóstico dos resíduos
plot(fit2)

# obtendo os resíduos do modelo anova ajustado
residuo <- residuals(fit2)
# teste de normalidade dos resíduos de Shapiro-Wilk
shapiro.test(residuo)


# modelo de regressão múltipla com interação tripla

mod3 <- lm(formula = mpg ~ hp*wt*cylf, data = mtcars)
# resumo dos resultados do ajuste do modelo
summary(mod3)

# gráficos de diagnóstico dos resíduos
plot(mod3)

# obtendo os resíduos do modelo anova ajustado
residuo <- residuals(mod3)
# teste de normalidade dos resíduos de Shapiro-Wilk
shapiro.test(residuo)


fit3 <- lm(formula = price ~ carat*depth*cutf, data = diamonds)
# resumo dos resultados do ajuste do modelo
summary(fit3)

# gráficos de diagnóstico dos resíduos
plot(fit3)

# obtendo os resíduos do modelo anova ajustado
residuo <- residuals(fit3)
# teste de normalidade dos resíduos de Shapiro-Wilk
shapiro.test(residuo)



# 17° ANOVA ---------------------------------------------------------------


# Anova de 1 fator

# Delineamento inteiramente ao acaso

# Criando o conjunto de dados
cars <- data.frame(type = factor(c(rep('subcompact',10),rep('compact',10), 
                                   rep('midsize',10), rep('full size',10))),
                   obs = c(3,5,3,7,6,5,3,2,1,6,1,3,4,7,5,6,3,2,1,7,
                           4,1,3,5,7,1,2,4,2,7,3,5,7,5,10,3,4,7,2,7)
)

# ajustando o modelo anova
anova <- aov(obs~type, data = cars)

# verificando a normalidade dos resíduos

# obtendo os resíduos do modelo anova ajustado
residuo <- residuals(anova)

# gráfico de normalidade QQ-Plot
qqnorm(residuo)
qqline(residuo, col = 'blue')
# teste de normalidade Shapiro-Wilk
shapiro.test(residuo)

# verificando a homogeneidade da variância

# obtendo os valores ajustados pelo modelo
ajustado <- fitted(anova)

# gráfico dos valores ajustados vs resíduos do modelo
plot(ajustado, residuo)
abline(h=0, col='blue')
# teste de homogeneidade da variância de Bartlett
bartlett.test(obs~type, cars)

# resumo dos resultados do ajuste do modelo anova
summary(anova)


# Delineamento de blocos casualizados

# Criando o conjunto de dados
resp <- data.frame(algor = factor(c(rep(1,6), rep(2,6), rep(3,6), rep(4,6),
                                    rep(5,6), rep(6,6))),
                   bloc = factor(c(rep(1:6,6))),
                   proj = c(1244,21,82,2221,905,839,281,129,396,1306,336,910,
                            220,84,458,543,300,794,225,83,425,552,291,826,
                            19,11,-34,121,15,103,-20,35,-53,170,104,199))


# ajustando o modelo anova
anova2 <- aov(proj~algor+bloc, data = resp)

# verificando a normalidade dos resíduos

# obtendo os resíduos do modelo anova ajustado
residuo <- residuals(anova2)

# gráfico de normalidade QQ-Plot
qqnorm(residuo)
qqline(residuo, col='blue')
# teste de normalidade Shapiro-Wilk
shapiro.test(residuo)

# verificando a homogeneidade da variância

# obtendo os valores ajustados pelo modelo
ajustado <- fitted(anova2)

# gráfico dos valores ajustados vs resíduos do modelo
plot(ajustado, residuo)
abline(h=0, col='blue')
# teste de homogeneidade da variância de Bartlett
bartlett.test(proj~algor, data=resp)

# resumo dos resultados do ajuste do modelo anova
summary(anova2)

# teste de comparações múltiplas de Tukey
tukey <- HSD.test(anova2, 'algor')
tukey$groups


# Anova de 2 fatores 

# Delineamento inteiramente ao acaso

# Criando o conjunto de dados
quimico <- data.frame(temp = factor(c(rep(150,6), rep(160,6), rep(170,6))),
                      pres = factor(rep(c(rep(200,2),rep(215,2),rep(230,2)),3)),
                      rend = c(90.4,90.2,90.7,90.6,90.2,90.4,90.1,90.3,90.5,
                               90.6,89.9,90.1,90.5,90.7,90.8,90.9,90.4,90.1))

# gráfico do comportamento dos dados
ggplot(data = quimico, mapping = aes(x = temp,y = rend, fill = pres)) + 
  geom_boxplot()

# ajustando o modelo anova
anova3 <- aov(rend~temp*pres, data = quimico)

# verificando a normalidade dos resíduos

# obtendo os resíduos do modelo anova ajustado
residuo <- residuals(anova3)

# gráfico de normalidade QQ-Plot
qqnorm(residuo)
qqline(residuo, col='blue')

# teste de normalidade Shapiro-Wilk
shapiro.test(residuo)

# verificando a homogeneidade da variância

# obtendo os valores ajustados pelo modelo
ajustado <- fitted(anova3)

# gráfico dos valores ajustados vs resíduos do modelo
plot(ajustado, residuo)
abline(h=0, col='blue')
bartlett.test(rend~interaction(temp,pres), data = quimico)

# resumo dos resultados do ajuste do modelo anova
summary(anova3)

# efeito principal da temperatura
tukey_temp <- HSD.test(y = quimico$rend, trt = quimico$temp,
                       DFerror = anova3$df.residual,
                       MSerror = summary(anova3)[[1]][4,3],
                       alpha = 0.05, console = TRUE)

# efeito principal da pressão
tukey_pres <- HSD.test(y = quimico$rend, trt = quimico$pres,
                       DFerror = anova3$df.residual,
                       MSerror = summary(anova3)[[1]][4,3],
                       alpha = 0.05, console = TRUE)


# Delineamento de blocos casualizados

# carregando o conjunto de dados npk do pacote datasets do R
data("npk")

# ajustando o modelo anova
anova4 <- aov(formula = yield ~ block + N*P*K, data = npk)

# verificando a normalidade dos resíduos

# obtendo os resíduos do modelo anova ajustado
residuo <- residuals(anova4)

# gráfico de normalidade QQ-Plot
qqnorm(residuo)
qqline(residuo, col='blue')

# teste de normalidade Shapiro-Wilk
shapiro.test(residuo)

# verificando a homogeneidade da variância

# obtendo os valores ajustados pelo modelo
ajustado <- fitted(anova4)

# gráfico dos valores ajustados vs resíduos do modelo
plot(ajustado, residuo)
abline(h=0, col='blue')
bartlett.test(yield~interaction(N,P,K), data = quimico)

# resumo dos resultados do ajuste do modelo anova
summary(anova4)
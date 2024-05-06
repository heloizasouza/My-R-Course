
# 2° Telas e Atalhos no RStudio

# CTRL+ENTER : roda a(s) linha(s) selecionada(s) no script. O atalho mais utilizado.
# ALT+- : cria no script um sinal de atribuição (<-). Você o usará o tempo todo.
# CTRL+SHIFT+C : Comenta ou descomenta as linhas selecionadas de uma vez
# ALT+SHIFT+K : abre uma janela com todos os atalhos disponíveis.


# 10° Importação de dados

# O diretório de trabalho é a pasta em que o R vai procurar arquivos na hora de 
# ler informações ou gravar arquivos na hora de salvar objetos.

# identifica o diretório
getwd()
# muda de diretório
setwd("C:/Users/Heloiza/Desktop/Consultoria")
setwd("C:/Users/Heloiza/Desktop/Meu Curso")


# lendo dados do tipo csv na pasta do projeto
mtcars <- read.csv(file = "mtcars.csv")
# lendo dados do tipo excel -- precisa de pacote
library(readxl)
insetos <- read_xlsx(path = "C:/Users/Heloiza/Downloads/InsectSprays.xlsx", sheet = 1)
sprays <- read_xlsx(path = "OrchardSprays.xlsx")
# lendo dados do tipo txt
insetos <- read.table("C:/Users/Heloiza/Downloads/InsectSprays.txt", header = TRUE)
sprays <- read.table("C:/Users/Heloiza/Downloads/OrchardSprays.txt", header = TRUE)
# lendo dados do R ou de pacotes do R
data("diamonds") # conjunto de dados do pacote ggplot2
View(diamonds)


# 11° Funções básicas

# somatório
sum(mtcars$mpg)
# quantidade de observações o n da amostra
length(mtcars$mpg)
nrow(mtcars)
# média
mean(mtcars$mpg)
sum(mtcars$mpg)/length(mtcars$mpg)
# desvio padrão
sd(mtcars$mpg)
# variância
var(mtcars$mpg)
sd(mtcars$mpg)^2
# mediana
median(mtcars$mpg)
# amplitude dos dados
range(mtcars$mpg)
# quantis
quantile(x = mtcars$mpg)
quantile(x = mtcars$mpg, probs = seq(0,1,0.1))
# tabela de frequências absolutas
table(mtcars$cyl)
# tabela de frequências relativas
prop.table(table(mtcars$cyl))
table(mtcars$cyl)/length(mtcars$cyl)


# 12° Gráficos básicos

# gráfico de barras
tb1 <- round(prop.table(table(mtcars$cyl))*100, 2)
barplot(tb1, xlab = "Cilindradas", ylim = c(0,45), col = c(2,3,4))
# gráfico de dispersão
plot(mtcars$wt, mtcars$mpg)
# histograma
hist(mtcars$drat)
# boxplot
boxplot(count ~ spray, data = insetos, col = "lightgray")
boxplot(decrease ~ treatment, data = sprays, col = "bisque")
#ogiva de galton
install.packages("agricolae",dep=T)
library(agricolae)
g1<-graph.freq(mtcars$drat,plot=FALSE)
points<-ogive.freq(g1,col="red",frame=FALSE,
                   ylab="Frequência relativa acumulada", main="ogive")


# 13° Gráficos com pacote ggplot2

library(ggplot2)
# gráfico de barras
ggplot(data = sprays, mapping = aes(x = as.factor(treatment), y = decrease)) +
  geom_col(mapping = aes(fill = treatment)) +
  labs(x = "Tratamentos", y = "Diminuição do volume", fill = "Tratamentos") + 
  theme_minimal()
# gráfico de dispersão
ggplot(data = mtcars, mapping = aes(x = wt, y = mpg)) + 
  geom_point() +
  geom_abline(intercept = 37, slope = -5) # são os coeficientes do lm
# gráfico de alisamento
ggplot(data = mtcars, mapping = aes(x = wt, y = mpg)) + 
  geom_point() +
  geom_smooth(method = "lm")
# histograma
ggplot(data = diamonds, mapping = aes(x = price, fill = cut)) +
  geom_histogram() +
  theme_linedraw()
# boxplot
ggplot(data = diamonds, mapping = aes(x = color, y = carat)) + 
  geom_boxplot() +
  theme_light()
ggplot(data = insetos, mapping = aes(x = spray, y = count)) +
  geom_boxplot()


# 14° Correlação

rm(list=ls())
dados <- data.frame(renda=c(10,15,12,70,80,100,20,30,10,60),
                 nfilhos=c(8,6,5,1,2,2,3,2,6,1),
                 estudo=c(3,5,5,12,16,18,8,8,4,8))

# correlação entre a renda e quantidade de filhos
cor.test(dados$renda, dados$nfilhos)
# correlação entre o tempo de estudos e a quantidade de filhos
cor.test(dados$estudo,dados$nfilhos)
# correlação entre a renda e o tempo de estudos
cor.test(dados$renda,dados$estudo)
# obtém a matriz de correlação
matriz <- cor(dados)
# obtém o gráfico da matriz de correlação
library(corrplot)
corrplot(corr = matriz, method = 'number')
corrplot(corr = matriz, method = 'color', type = 'upper', addCoef.col = "white")


# 15° Regressão Linear

dados2 <- data.frame(leite = c(26, 25, 31,29, 27, 31,32,28,30,30),
                     chuva = c(23,21,28,27,23,28,27,22,26,25))

# gráfico de dispersão
plot(dados2$chuva,dados2$leite,pch=19,col="blue", 
     xlab="Precipitação pluviométrica",ylab="Produção de Leite")

# modelo linear ajustado
mod <- lm(leite~chuva, data = dados2)
summary(mod)

# gráfico de dispersão
plot(dados2$chuva,dados2$leite,pch=19,col="blue", 
     xlab="Precipitação pluviométrica",ylab="Produção de Leite")
#reta ajustada
abline(lm(leite~chuva, data = dados2),lwd=4,col="grey")
# valores ajustados
points(dados2$chuva,fitted(mod),col="red",pch=19)

diamonds$cutf <- as.factor(as.character(diamonds$cut))
diamonds$clarityf <- as.factor(as.character(diamonds$clarity))

# Modelo de Regressão Múltipla
mod1 <- lm(formula = price ~ carat + cutf + depth + clarityf, data = diamonds)
summary(mod1)
# análise de resíduos
plot(mod1)

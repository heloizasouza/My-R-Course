

# 2° Telas e Atalhos no RStudio -------------------------------------------



# CTRL+ENTER : roda a(s) linha(s) selecionada(s) no script. O atalho mais utilizado.
# ALT+- : cria no script um sinal de atribuição (<-). Você o usará o tempo todo.
# CTRL+SHIFT+C : Comenta ou descomenta as linhas selecionadas de uma vez
# ALT+SHIFT+K : abre uma janela com todos os atalhos disponíveis.



# 10° Importação de dados -------------------------------------------------



# O diretório de trabalho é a pasta em que o R vai procurar arquivos na hora de 
# ler informações ou gravar arquivos na hora de salvar objetos.

# identifica o diretório
getwd()
# mudança de diretório
setwd("C:/Users/Heloiza/Downloads")
setwd("C:/Users/Heloiza/Desktop/Consultorias/My-R-Course")


# lendo dados do tipo csv na pasta do projeto
mtcars <- read.csv(file = "mtcars.csv")

# lendo dados do tipo excel -- precisa de pacote
library(readxl)
insetos <- read_xlsx(path = "C:/Users/Heloiza/Downloads/InsectSprays.xlsx", sheet = 1)
sprays <- read_xlsx(path = "OrchardSprays.xlsx")

# lendo dados do tipo txt
insetos <- read.table("C:/Users/Heloiza/Downloads/InsectSprays.txt", header = TRUE)
sprays <- read.table("OrchardSprays.txt")

# lendo dados do R ou de pacotes do R
data("diamonds") # conjunto de dados do pacote ggplot2
View(diamonds)



# 11° Funções básicas -----------------------------------------------------


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
# mínimo e máximo de um vetor
range(mtcars$mpg)
# quantis
quantile(x = mtcars$mpg)
quantile(x = mtcars$mpg, probs = seq(0,1,0.1))
# tabela de frequências absolutas
table(mtcars$cyl)
# tabela de frequências relativas
prop.table(table(mtcars$cyl))
table(mtcars$cyl)/length(mtcars$cyl)
# construindo uma função
X_barra <- function(X) {
  media <- sum(X)/length(X)
  return(media)
}



# 12° Gráficos básicos ----------------------------------------------------


# gráfico de barras
tb1 <- round(prop.table(table(mtcars$cyl))*100, 2)
barplot(tb1, xlab = "Cilindradas", ylim = c(0,45), col = c(2,3,4))
data("longley")
barplot(Employed ~ Year, data = longley)

# gráfico de dispersão
plot(mtcars$wt, mtcars$mpg)

# gráfico de linhas 
plot(Employed ~ Year, data = longley, type = "l")

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



# 13° Gráficos com pacote ggplot2 -----------------------------------------


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

# gráfico de linhas
ggplot(data = longley, mapping = aes(x = Year, y = Employed)) +
  geom_line()

# histograma
ggplot(data = diamonds, mapping = aes(x = price, fill = cut)) +
  geom_histogram() +
  theme_linedraw()

# boxplot
ggplot(data = diamonds, mapping = aes(x = color, y = carat)) + 
  geom_boxplot() +
  theme_light()

# gráfico de violino
ggplot(data = insetos, mapping = aes(x = spray, y = count)) +
  geom_violin()
ggplot(data = insetos, mapping = aes(x = spray, y = count)) +
  geom_boxplot()

# gráfico de alisamento
ggplot(data = mtcars, mapping = aes(x = wt, y = mpg)) + 
  geom_point() +
  geom_smooth(method = "lm")


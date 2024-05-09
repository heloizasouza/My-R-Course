
# 1° O que são Projetos no RStudio? ---------------------------------------


# Um projeto nada mais é do que uma pasta no seu computador. 
# Nessa pasta, estarão todos os arquivos que você usará ou criará na sua análise.
# A principal razão de utilizarmos projetos é organização.



# 2° Telas e Atalhos no RStudio -------------------------------------------


# CTRL+ENTER : roda a(s) linha(s) selecionada(s) no script. O atalho mais utilizado.
# ALT+- : cria no script um sinal de atribuição (<-). Você o usará o tempo todo.
# CTRL+SHIFT+C : Comenta ou descomenta as linhas selecionadas de uma vez
# ALT+SHIFT+K : abre uma janela com todos os atalhos disponíveis.



# 3° Pedindo ajuda no R ---------------------------------------------------


# pesquisa de ajuda basica
?mean
help("mean")
help(mean)
?base
# para pesquisar um pacote ou função não instalado
??mutate
??dplyr
# para abrir a lista de funções completa do pacote
library(help = "base")
library(help = "dplyr")



# 4° uso do R como uma calculadora ----------------------------------------



#soma
2+3
#subtração
100-37
#produto
2.5*2
#divisão
100/7
#potência
25^5         
#raiz quadrada
sqrt(61)
#arredondamento
round(sqrt(61),2)           
# resto da divisão de 5 por 3
5 %% 3
# parte inteira da divisão de 5 por 3
5 %/% 3

round()



# 5° Criando objetos ------------------------------------------------------

 

# Existem diferentes maneiras de declarar um objeto
x<-10
x=10
X <- 6
w <- c(1,2,3,4,5)
z <- 2*w; z    # o argumento ; é usado para separar comandos

# Regras para nomear objetos
# Permitido

x <- 1
x1 <- 2
objeto <- 3
meu_objeto <- 4
meu.objeto <- 5

# Não permitido

1x <- 1
_objeto <- 2
meu-objeto <- 3



#  6° Listando e removendo objetos ----------------------------------------


ls()
x;w
rm(x,w)					#remove os objetos x e w
x               #observe a resposta
rm(list=ls())	
ls()

# Para limpar a janela do Console, basta clicar com o mouse
# sobre algum local do Console para ativar o cursor e
# em seguida, pressionar Ctrl+l no teclado.



# 7° Classes dos objetos --------------------------------------------------



x <- 1
class(x)
y <- "a"
class(y)
z <- factor(c("Ensino Fundamental", "Ensino Medio", "Graduação"))
class(z)
a <- TRUE
class(a)
class(mtcars)

nome <- "Julio"
valor <- 2
# operação matemática com o objeto "valor"
3+valor
# observe a mensagem
3+nome
paste0("O ", nome)



# 8° Data Frames ----------------------------------------------------------



# Os data frames são equivalentes a uma tabela de dados ou uma planilha do Excel.
# A principal característica de um data frame é possuir linhas e colunas.

data("mtcars")
head(mtcars)
data(USArrests, "VADeaths")
?USArrests
?VADeaths
head(USArrests)
head(VADeaths)




# 9° Pacotes --------------------------------------------------------------


install.packages("ggplot2")
library(ggplot2)


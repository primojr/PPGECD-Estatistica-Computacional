#
# Lista I - Execicio PGMAT0061 
# 
library(tidyverse)
# Qustão 1
## Criar uma função para calcular o coeficiente de person
x <- c(0.06,-0.55,-1.41,-1.57,0.07,-0.65,0.73,0.73,-0.22,0.27)
y <- c(-0.46, 0.1, -2.51, -2.31, -1.06, -0.67, 0.72, 0.5, 0.4, 0.77)

cor(x, y)

correlacao <- function(x, y) {
  # numerador  
  numerador = sum(
      (x - mean(x)) *
        (y - mean(y))
    )
  # denominador 
  denominador <- sqrt(
    sum((x - mean(x))**2) * 
      sum((y - mean(y))**2)
  )
  r_person = numerador / denominador
  
  print(
    paste("Coef. de correlação de person é: ", 
    round(r_person, digits = 7)
  )
)
}
correlacao(x,y)

# Questão 2 
## Numa determinada localidade, a distribuição de renda (em unidades monetárias, u.m.) é uma
## variável aleatória X com função de distribuição de probabilidade:
## 

#f.d.p
f_X <- function(x) {
  dplyr::case_when(
    x >= 0 & x <= 2 ~ (1/10*x + 1/10)
  , x  > 2 & x <= 6 ~ (-3/40*x + 9/20)
  , TRUE ~ 0
  )
}

# Q2 - a
## Mostre que é uma função de propabilidade 
# Resposta:

# a.1 Para todo x, f(x) >= 0
# Garantindo para os valores minimo e maximo
min_x = 0
max_x = 6
f_X(min_x) > 0 # TRUE
f_X(max_x) > 0 # TRUE

#
# a.2 A Função acumulada no ponto maximo é igual a zero
F_X <- function(x) {
  if_else(x >= 0 & x <= 6, integrate(f_X, lower = 0, upper = x)$value, 0)  
}

F_X(max_x) == 1 # TRUE


# a.3 A soma de todas as probabilidades é igual a 1
sum(sapply(0:6, f_X))

# Q2 - b Criar grafico da função de distribuição de probabilidade
x <- seq(0, 6, by = 0.1)
y <- sapply(x, f_X)

df <- data.frame(x = x, y = y)

ggplot(df, aes(x = x, y = y)) +
  geom_line() +
  labs(
    title = "Função de distribuição de probabilidade"
    ,x = "x"
    ,y = "f(x)"
    )

# Q2 - c P(X >= 4.5)
p_4.5 <- 1 - F_X(4.5)
p_4.5

# Q3 - D Calcule E(x) e Var(x) 
E_X <- integrate(function(x) x * f_X(x), lower = 0, upper = 6)$value
E_X

Var_X <- integrate(function(x) (x - E_X)^2 * f_X(x), lower = 0, upper = 6)$value
Var_X





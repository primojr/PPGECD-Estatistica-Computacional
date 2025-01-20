# Lista de exercicio 1

# Q1 - Gerador RANDU
# Criar uma funcão para Gerador RANDU em R
randu <- function(n, seed = 1, modulo = 2^31) {
  x <- seed
  u <- numeric(n) 
  for (i in 1:n) {
    x <- (65539 * x) %% modulo
    u[i] <- x / modulo
  }
  return(u)
}

randu(1000) |> hist()

# Q2 - Gerador Aleatorio - sequencia
gerador_seq <- function(n) {
  x <- numeric(n)  
  x[1] <- 23
  x[2] <- 66
  
  for (i in 3:n) {
    x[i] <- (3 * x[i - 1] + 5 * x[i - 2]) %% 100
  }
  
  u <- x / 100
  
  list(xn = x, un = u)
}

n <- 14
resultado <- gerador_seq(n)
resultado$xn

# Q3 - Função de probabilidade
F_x <- function() {
  repeat {
    u <- runif(1)
    if (u <= 1/3) {
      return(1)
    } else {
      return(2)
    }
  }
}

# a) n = 100  
n = 100
resultado <- purrr::map_dbl(1:n, ~ F_x())
resultado |> janitor::tabyl()

# b) n = 1000
n = 1000
resultado <- purrr::map_dbl(1:n, ~ F_x())
resultado |> janitor::tabyl()

# c) n = 10000
n = 10000
resultado <- purrr::map_dbl(1:n, ~ F_x())
resultado |> janitor::tabyl()


dist.normal <- function() {

  y1 <- rexp(n = 1, rate = 1)
  y2 <- rexp(n = 1, rate = 1)
  
  if(y2 > ((1 - y1)^2)/2) {
    u <- runif(n = 1, min = 0, max = 1)
    x <- ifelse(u <= .5, y1, -y1)
  }
  return(x)
}

# Metodo da inversão

# 1. Gerador acoplado 
# Gerador congruencial
GeradorWichmannHill <- function(N = 1000, x0 = 1, y0 = 1, z0 = 1) {
  x = x0
  y = y0
  z = z0
  
  u = numeric(N)
  
  for (i in 1:N) {
    x = (171*(x - 1))%%30269
    y = (172*(y - 1))%%30307
    z = (173*(y - 1))%%30323
    
    u[i] = (x/30269 + y/30307 + z/30323)%%1
  }
  return(u)
}

GeradorWichmannHill(100) |> hist()

# Exemplos

# Uniforme
# U ~ U(a, b)
dist.Uniforme <- function(N = 1000, a = 0, b = 1,
                          x1 = 1, x2 = 2, x3 = 3) {
  u = runif(N)
  x = a + u * (b - a)
  return(x)
}

Ns <- c(100, 1000, 10000)
par(mfrow = c(1, 3))

histograma <- function(N, a, b) {
  x <- dist.Uniforme(N, a, b)
  hist(x, freq = FALSE, col = "lightblue", border = "black", 
       main = paste("N =", length(x)))
  curve(dunif(x, a, b), add = TRUE, col = "red")
}

purrr::walk(Ns, a = 1,b = 6, histograma)

# Logistica
# U ~ Log(mu, s)
dist.Logistica <- function(N = 1000, mu = 0, s = 1) {
  u <- runif(N)
  x <- mu + s * log(u/(1 - u))
  return(x)
}

histograma <- function(N, mu, s) {
  x <- dist.Logistica(N, mu, s)
  hist(x, freq = FALSE, col = "lightblue", border = "black", 
       main = paste("N =", length(x)))
  curve(dlogis(x, mu, s), add = TRUE, col = "red")
}

par(mfrow = c(1, 3))
Ns <- c(100, 1000, 10000)
purrr::walk(Ns, mu = 5, s = 1, histograma)


# Poisson 
# U ~ Poisson(lambda)
dist.Poisson <- function(lambda) {
  prod.u <- 1
  k <- 0
  
  while (prod.u >= exp(-lambda)) {
    u <- runif(1)  
    prod.u <- prod.u * u  
    k <- k + 1  
  }
  return(k - 1)  
}

par(mfrow = c(1, 3))

Ns <- c(100, 1000, 10000) 
lambda <- 5 

x <- map_dbl(1:Ns[3], ~ dist.Poisson(lambda))
hist(x, freq = FALSE, col = "lightblue", border = "black", 
     main = paste("N =", length(x)))
curve(dpois(x,lambda = 5), add = TRUE, col = "red")

rpois(10000, lambda) |> 
  hist(freq = FALSE,
       col = "lightblue",
       border = "black",
       main = paste("Poisson R: N =", 10000)
       )


## Metodo polar 
gerar_normal <- function() {
  repeat {
    u1 <- runif(1)*2 - 1
    u2 <- runif(1)*2 - 1
    
    u <- u1^2 + u2^2
    
    if(u < 1) {
      x <- u1 * sqrt(-2 * log(u) / u)
      return(x)
    }
  }
}

n <- 10000
x <- purrr::map_dbl(1:n, ~gerar_normal())

hist(x, probability = TRUE
     , col = "lightblue", main = "Hist. n = ")
curve(dnorm(x, mean = 0, sd = 1), add = TRUE, col = "red")

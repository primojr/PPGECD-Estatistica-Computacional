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
  u
}

GeradorWichmannHill(100) |> hist()

# Exemplos

# Uniforme
# U ~ U(a, b)
dist.Uniforme <- function(N = 1000, a = 0, b = 1,
                          x1 = 1, x2 = 2, x3 = 3) {
  u = GeradorWichmannHill(N)
  a + u * (b - a)
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
  u = GeradorWichmannHill(N)
  mu + s * log(u/(1 - u))
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


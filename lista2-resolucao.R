### Lista 2 - Metodo de monte carlo MMC:

# Q1 - MMC 
# X ~ EXP(1)
# Estimar E[X | X < 0.05] 
fx <- function(x) {
 y <- exp(-x)/(1-exp(-0.05))
 return(y)
}

n = 1000
x <- rexp(n, rate = 1)
x <- fx(x)
x <- x[x < 0.05]
mean(x)

# Q2 - Calcular integrais pelo MMC 
mmc <- function(fx, a, b, n) {
 x <- runif(n, min = a, max = b)
 y <- fx(x)
 e_x = mean(y) * (b - a)
 I_x = integrate(fx, lower = a, upper = b)
 erro = round(abs(e_x - I_x$value), 4)
 resultado = list(e_x = e_x, I_x = I_x, erro = erro)
 return(resultado)
}

# a) Integral de 0 a 1 de exp(e^x) dx
fx <- function(x) {
 y <- exp(exp(x))
 return(y)
}

n = 10000
mmc(fx, 0, 1, n)

# b) Integral de -2 a 2 de exp(x+x^2) dx
fx <- function(x) {
 y <- exp(x + x^2)
 return(y)
}

n = 1000000
mmc(fx, -2, 2, n)

# c) Integral de 0 a inf de x(1+x)^-2 dx
fx <- function(x) {
 y <- x*((1+x)^-2)
 return(y)
}

n = 1000
mmc(fx, 0, n, n)

# d) Integral de -inf a inf de e^-x^2 dx
fx <- function(x) {
 y <- exp(-x^2)
 return(y)
}

n = 100
mmc(fx, -n^2, n^2, n)


# e) Integral dupla de 0 a inf e 0 a x de e^-(x+y) dx dy  
fz <- function(x) {
  x <- runif(n, min = 0, max = n)
  y <- runif(n, min = 0, max = x)
  z <- exp(-(x+y))
  return(z)
}

n = 1000
mmc(fz, 0, n, n)

# Q3 - MMC - Cov(U, e^U)
# Função para simular Cov(U, e^U)
simular_cov <- function(n) {
  U <- runif(n, min = 0, max = 1)
  e_U <- exp(U)
  E_U_eU <- mean(U * e_U)      # E[U * e^U]
  E_U <- mean(U)               # E[U]
  E_eU <- mean(e_U)            # E[e^U]
  
  # Calcular a covariância
  cov_U_eU <- E_U_eU - E_U * E_eU
  cov_real <- cov(U, e_U)
  return(list('cov_est' = cov_U_eU, 'cov' = cov_real))
}

n <- 1000
resultado_simulacao <- simular_cov(n)
glue::glue("Estimativa de Monte Carlo para Cov(U, e^U): {resultado_simulacao$cov_est} 
            Valor exato de Cov(U, e^U): {resultado_simulacao$cov}
            Erro absoluto: {abs(resultado_simulacao$cov_est - resultado_simulacao$cov)}"
           )

# Q4 Integral de 0 a inf x^2sen(pix)exp(-x/2) dx
fx <- function(x) {
 y <- x^2 * sin(pi*x) * exp(-x/2)
 return(y)
}

n = 1000
mmc(fx, 0, n^2, n)

# Q5 Gerar exp
e_exp <- function(n, l = 100) {
  u <- runif(n, min = 0, max = 1)
  x <- -log(1 - u)
  e_x <- mean(x <= l)
  return(e_x)
}

# Adaptando a função com um limite, podemos ter a probabilidade 
e_exp(10000, l = .4)

# Q6 - Poisson-exponencial
# Gerar amostra
dpoisexp <- function(y, theta, lambda) {
  num <- theta * lambda * exp((-lambda * y) - theta * exp(-theta * y))
  denom <- 1 - exp(-theta)
  return(num / denom)
}


# Parâmetros fixos
theta <- 1
lambda <- 1
n <- 100

p = purrr::map_dbl(1:n, ~dpoisexp(runif(1), theta, lambda))
mean(p)






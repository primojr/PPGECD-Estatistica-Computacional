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
# a) Integral de 0 a 1 de exp(e^x) dx
fx_mmc <- function(n, a, b) {
  x <- runif(n, min = a, max = b)
  gx <- function(x) {
    y <- exp(exp(x))
    return(y)
  }
  hx <- (b-a)*gx(x)/(b-a)
  e_x <- mean(hx)
  I_x <- integrate(fx, a, b)$value
  erro <- abs(I_x - e_x)
  return(list(e_x=e_x,I_x=I_x, erro = erro))
}

n = 1000
fx_mmc(n, 0, 1)


# b) Integral de -2 a 2 de exp(x+x^2) dx
fx_mmc <- function(n, a, b) {
  x <- runif(n, min = a, max = b)
  gx <- function(x) {
    y <- exp(x+x^2)
    return(y)
  }
  hx <- (b-a)*gx(x)/(b-a)
  e_x <- mean(hx)
  I_x <- integrate(fx, a, b)$value
  erro <- abs(I_x - e_x)
  return(list(e_x=e_x,I_x=I_x, erro = erro))
}

n = 1000
fx_mmc(n, 0, 1)

# c) Integral de 0 a inf de x(1+x)^-2 dx
fx_mmc <- function(n, a, b) {
  x <- rexp(n = n, rate = 1)
  fx <- function(x) {
    y <- x*((1+x^2)^-2)
    return(y)
  }
  hx <- fx(x)/exp(-x) 
  e_x <- mean(hx) |> round(4)
  I_x <- integrate(f = fx, lower = a,upper = b)$value |> round(4)
  erro <- abs(I_x - e_x) |> round(4)
  return(list(e_x=e_x,I_x=I_x, erro = erro))
}

n = 10000
fx_mmc(n, a = 0, b = Inf)

# d) Integral de -inf a inf de e^-x^2 dx
fx_mmc <- function(n, a, b) {
  x <- rnorm(n = n, mean = 0, sd = 1)
  fx <- function(x) {
    y <- exp(-x^2)
    return(y)
  }
  hx <- fx(x)/((exp(-0.5*x^2))/sqrt(2*pi))
  e_x <- mean(hx) |> round(4)
  I_x <- integrate(f = fx, lower = a,upper = b)$value |> round(4)
  erro <- abs(I_x - e_x) |> round(4)
  return(list(e_x=e_x,I_x=I_x, erro = erro))
}

n = 10000
fx_mmc(n, a = -Inf, b = Inf)


# e) Integral dupla de 0 a inf e 0 a x de e^-(x+y) dx dy  
fx_mmc <- function(n, a, b) {
  x <- rexp(n = n, rate = 1)
  y = runif(n, min = 0, max = x)
  gxy <- function(x, y) {
    z <- exp(-(x+y))
    return(z)
  }
  hx <- gxy(x,y)/(exp(-x)*x)
  e_x <- mean(hx) |> round(4)
  return(list(e_x=e_x))
}

n = 1000
fx_mmc(n, a = 0, b = Inf)

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
fx_mmc <- function(n, a, b) {
  x <- rexp(n = n, rate = 1)
  gx <- function(x) {
    y <- x^2*sin(pi*x)*exp(-x/2)
    return(y)
  }
  hx <- gx(x)/(exp(-x))
  e_x <- mean(hx) |> round(4)
  return(list(e_x=e_x))
}

n = 10000
fx_mmc(n, a = 0, b = Inf)

# Q5 Gerar exp
e_exp <- function(n, l = 100) {
  u <- runif(n, min = 0, max = 1)
  x <- -log(1 - u)
  e_x <- mean(x <= l)
  return(e_x)
}

# Adaptando a função com um limite, podemos ter a probabilidade até um certo ponto
e_exp(10000, l = 100)

# Q6 - Poisson-exponencial
# Gerar amostra
dpoisexp <- function(y, theta, lambda) {
  num <- theta * lambda * exp((-lambda * y) - theta * exp(-theta * y))
  denom <- 1 - exp(-theta)
  return(num / denom)
}

m.rejeicao <- function(n, theta, lambda) {
  c <- theta / lambda
  
  amostra <- numeric(n) 
  count <- 0 
  
  while (count < n) {
    # 1. Amostra y ~ g(y)
    y <- rexp(1, rate = lambda)
    
    # 2. u ~ Uniform(0, 1)
    u <- runif(1)
    
    # 3.  u <= f(y) / (c * g(y))
    if (u <= dpoisexp(y, theta, lambda) / (c * y)) {
      count <- count + 1
      amostra[count] <- y
    }
  }
  return(amostra)
}


# Parâmetros fixos
theta <- 1
lambda <- 1
n <- 10000

# Gerar amostra
a <- m.rejeicao(n, theta, lambda)
a |> hist(probability = TRUE, col = "lightblue")
p = purrr::map_dbl(1:n, ~dpoisexp(runif(1), theta, lambda))
mean(p)

resultado <- optim(
  par = c(theta,lambda),
  fn = dpoisexp(a, theta, lambda),
  method="L-BFGS-B",
  hessian = TRUE,
  lower = c(0.0001, 0.0001)
  )

resultado |> print()
sd <- resultado$hessian |> solve() |> diag() |> sqrt()
names(sd) <- c("theta", "lambda")
sd |> print()

erro =(1/n)*sd
erro |> print()

est_par <- cbind(resultado$par,sd, erro)

# limite de confiança de 95%
est_par <- cbind(est_par, est_par[,1] - 1.96*est_par[,3], est_par[,1] + 1.96*est_par[,3])
colnames(est_par) <- c("valor", "sd", "erro", "lim_inf", "lim_sup")
est_par |> print()






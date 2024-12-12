#
# Metodo da rejeição - Geral
#
metodo_rejeicao <- function() {
  repeat {
    x <- runif(n = 1, min = -1, max = 1)
    u <- runif(n = 1, min = 0, max = 1)
    
    if (u <= 1-x^2)
      {return(list(x = x, contagem = 'Aceito'))}
      else
        {return(list(x = NA, contagem = 'Rejeito'))}
  }
}

n = 10000
metodo_rejeicao()
amostra <- purrr::map_df(1:n, ~metodo_rejeicao())  

amostra$contagem |> table() |> prop.table()

amostra$x |> hist(probability = TRUE, main = paste('N = ', n))
curve(3/4*(1-x^2), from = -1, to = 1, col = "red", lwd = 1, add = TRUE)

#
# Metodo da rejeição - “squeezing” 
#
metodo_rejeicao_squeezing <- function() {
  repeat {
    x <- runif(n = 1, min = -1, max = 1)
    u <- runif(n = 1, min = 0, max = 1)
    
    if (u <= 1-abs(x))
      {return(list(x = x, contagem = 'Aceito Cond.1'))}
      else if(u <= 1-x^2)
      {return(list(x = x, contagem = 'Aceito Cond.2'))}
      else
        {return(list(x = NA, contagem = 'Rejeito'))}
  }
}

n = 100000
metodo_rejeicao()
amostra <- purrr::map_df(1:n, ~metodo_rejeicao_squeezing())  

amostra$contagem |> table() |> prop.table()

amostra$x |> hist(probability = TRUE)
curve(3/4*(1-x^2), from = -1, to = 1, col = "red", lwd = 1, add = TRUE)

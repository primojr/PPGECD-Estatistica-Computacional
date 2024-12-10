metodo_rejeicao <- function() {
  repeat {
    x <- runif(n = 1, min = -1, max = 1)
    u <- runif(n = 1, min = 0, max = 1)

    fx = dplyr::if_else(u <= 1-x^2, x, NA )  
     c = dplyr::if_else(is.na(fx), 0, 1)
    
    return(list(x = fx, c = c)) 
  }
}

n = 10000
metodo_rejeicao()
amostra <- purrr::map_df(1:n, ~metodo_rejeicao())

amostra$c |> janitor::tabyl()

amostra$x |> hist(probability = TRUE)
curve(3/4*(1-x^2), from = -1, to = 1, col = "red", lwd = 1, add = TRUE)

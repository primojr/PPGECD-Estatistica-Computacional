# Carregar pacotes
library(tidyverse)
library(furrr)

# Config. Maquina
plan(multisession, workers = parallel::detectCores() - 1)
message("Number of parallel workers: ", nbrOfWorkers())

# df exemplo: Idade Normal(50,10)
set.seed(123)
df <- tibble(
  id = 1:1e6,
  idade = rnorm(1e6, mean = 50, sd = 10)
)

# Definir lotes 
num_batches <- 100  
batch_size <- nrow(df) %/% num_batches  

# lista de subamostras 
batches <- map(1:num_batches, ~ df %>% slice_sample(n = batch_size))

# Função de log-verossimilhança da distribuição Normal
log_verossimilhança <- function(par, dados) {
  mu <- par[1]
  sigma <- par[2]
  -sum(dnorm(dados, mean = mu, sd = sigma, log = TRUE))  # Negativo porque `optim()` minimiza
}

# Função para rodar `optim()` em um minibatch
ajustar_parametros <- function(df_lote) {
  resultado <- optim(
    par = c(mean(df_lote$idade), sd(df_lote$idade)),  # Chute inicial
    fn = log_verossimilhança, 
    dados = df_lote$idade, 
    method = "BFGS"
  )
  
  tibble(
    media_estimada = resultado$par[1],
    desvio_est = resultado$par[2]
  )
}

# Rodar `optim()` em paralelo 
resultados <- future_map_dfr(batches, ajustar_parametros)

# Fechar conexção
plan(sequential)

# Estimativas
media_final <- mean(resultados$media_estimada)
desvio_final <- mean(resultados$desvio_est)

# Exibir os resultados finais
cat("Média Estimada:", media_final, "\n",
    "Média Real:", df$idade %>% mean(), "\n",
    "--", "\n",
    "Desvio Padrão Estimado:", desvio_final, "\n",
    "Desvio Padrão Real:", df$idade %>% sd()
    )



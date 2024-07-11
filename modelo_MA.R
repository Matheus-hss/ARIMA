# Carregar pacotes
library(rbcb)
library(dplyr)
library(tsibble)
library(fabletools)
library(fable)
library(ggplot2)

# Carregar dados
y <- rbcb::get_series(
  code = c("y" = 433),
  start_date = "2010-01-01"
)

# Tratar dados
y_ts <- y |>
  dplyr::mutate(date = tsibble::yearmonth(date)) |>
  tsibble::as_tsibble(index = date)

# Separar amostras
y_teste <- tail(y_ts, 12)
y_treino <- dplyr::filter(y_ts, !date %in% y_teste$date)

# Estimar modelo
set.seed(1984)
modelo <- fabletools::model(
  .data = y_treino, 
  ma = fable::ARIMA(y ~ pdq(p = 0, d = 0) + PDQ(P = 0, D = 0, Q = 0))
)

# Sumário do modelo
fabletools::report(modelo)

# Produzir previsões
previsao <- fabletools::forecast(modelo, h = nrow(y_teste))

# Acurácia de treino e teste
dplyr::bind_rows(
  fabletools::accuracy(modelo),
  fabletools::accuracy(previsao, y_teste)
)

# Gráfico de valores observados e estimados/previstos
previsao |>
  fabletools::autoplot(y_ts) +
  ggplot2::geom_line(
    mapping = ggplot2::aes(y = .fitted),
    color = "#b22200",
    data = fabletools::augment(modelo)
  ) +
  ggplot2::labs(
    title = "Brasil: Taxa de Inflação - IPCA (modelo de previsão MA)",
    subtitle = "Var. % mensal",
    x = NULL,
    y = NULL,
    caption = "Dados: IBGE | Elaboração: Matheus"
  )

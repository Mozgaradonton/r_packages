#' Makes a list of a set of simple models forecasts 
#'
#' This function creates a set of forecasts getting from the simple models
#' @param ts Time series to forecast on
#' @param hor Forecast horizon
#' @keywords forecast
#' @export
#' @examples
#' simple_model_forecasts(ts, 1)

simple_model_forecasts <- function(ts, hor) {
  list(snaive = snaive(y = ts, h = hor),
       ses = ses(y = ts, h = hor),
       ets = ets(y = ts) %>% forecast(h = hor),
       arima = auto.arima(y = ts) %>% forecast(h = hor),
       tslm = tslm(formula = ts ~ trend + season) %>% forecast(h = hor),
       stlm = stlf(y = ts, h = 1))
  
}


#' Clculate forecast using weighted ensemble method
#'
#' This function calculates forecast basing on the weighted ensemble of simple methods from the function simple_model_forecasts
#' @param qty_ts Time series to forecast on
#' @param N Number of holdouts to test simple models to define weights
#' @param forecast_horizon Forecast horizon
#' @keywords forecast
#' @export
#' @examples
#' weighted_ensemble_forecast(ts, 3, 18)

weighted_ensemble_forecast <- function(qty_ts, N, forecast_horizon) {
  hist_ts <- map(seq(N), ~head(qty_ts, -.))
  actual_values <- map(seq(N), ~tail(qty_ts, .)) %>% map(1)
  
  mape_matrix <- hist_ts %>% map(simple_model_forecasts, 1) %>% map(map_dbl, 'mean') %>% 
    map2(.x = ., .y = actual_values, ~ map2_dbl(.x = .x, .y = .y, ~ TSPred::sMAPE(prediction = .x, actual = .y)))  %>% simplify2array()
  
  scores <- apply(mape_matrix, 1, mean)
  weights <- 1 / (scores^2)
  weights <- weights / sum(weights)
  
  
  forecast <- simple_model_forecasts(ts = qty_ts, hor = forecast_horizon) %>% map('mean')
  forecast_df <- forecast %>% reduce(cbind)
  colnames(forecast_df) <- names(forecast)
  forecast_df <- sweep(forecast_df, MARGIN = 2, weights, `*`)
  forecast_df <- round(apply(forecast_df, MARGIN = 1, sum))
  
  list(forecast = tibble(date = ts_tbl(forecast$snaive) %>% pull(time), qty_sales_forecast = forecast_df),
       mape_matrix = mape_matrix,
       weights = weights)
}

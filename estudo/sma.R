library(BT)
library(tidyverse)

SMA_CrossOver <- R6::R6Class(
  "SMA_CrossOver",
  inherit = Backtest,
  public = list(
    begin = function(n = 14L) {
      self$data <- petr4 |>
        mutate(sma = TTR::SMA(close, n = n)) |>
        mutate(buySignal = crossover(close, sma)) |>
        mutate(sellSignal = crossover(sma, close))

      self
    },
    on_bar = function(current_bar) {
      if (self$broker$position == 0L) {
        if (self$data$buySignal[current_bar]) {
          self$buy(100L)
        }
      } else {
        if (self$data$sellSignal[current_bar]) {
          self$sell(100L)
        }
      }
    }
  )
)

bt <- SMA_CrossOver$new(broker = Broker$new(cash = 1e6))$
  run(n = 21L)




DonchianChannelBreakOut <- R6::R6Class(
  "DonchianChannelBreakOut",
  inherit = Backtest,
  public = list(
    stop = NA_real_,
    begin = function(n = 14L, ATR_n = 14L, xstop = 2) {
      self$data <- petr4 |>
        mutate(suport = lag(TTR::runMax(high, n = n))) |>
        mutate(atr = TTR::ATR(.[,c("high", "low", "close")], n = ATR_n)) |>
        mutate(stop_loss = low - atr * xstop) |>
        mutate(buySignal = crossover(close, suport))
      self$stop <- NA_real_
      self
    },
    on_bar = function(current_bar) {
      if (self$broker$position == 0L) {
        self$buy_start(100L, start_price = self$data$suport[current_bar])
        self$stop <- NA_real_
      } else {
        self$stop <- max(self$stop, self$data$stop_loss[current_bar], na.rm = TRUE)
        self$sell_stop(100L, stop_price = self$stop)
      }
    }
  )
)


bt <- DonchianChannelBreakOut$
  new()$
  run(n = 21)

# Loading libraries
library(tidyverse)
library(tidyquant)

# Setting options
options(tibble.width = Inf)

# Downloading data for the mentioned Stocks and writing it as a csv
tidyquant::tq_get(c("STX", "FTV", "NOW", "IT", "AMD"),
                  from = "2019-01-01",
                  to = lubridate::today()) %>%
  dplyr::select(symbol, date, close) %>%
  readr::write_csv("Stock Price Data.csv")
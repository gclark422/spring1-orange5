# Loading libraries
library(tidyverse)
library(gurobi)
library(prioritizr)
library(scales)

# Setting options
options(tibble.width = Inf)
windowsFonts("tw_cen_mt" = windowsFont("Tw Cen MT"))

# Reading data
dat <- readr::read_csv("Stock Price Data.csv")

# Calculating the daily returns
dat <-  dat %>%
  dplyr::arrange(symbol, date) %>%
  dplyr::group_split(symbol) %>%
  purrr::map_df(~.x %>%
                  dplyr::mutate(lag_close = dplyr::lag(close),
                                returns = (close - lag_close) / lag_close) %>%
                  dplyr::filter(!is.na(returns)) %>%
                  dplyr::select(-lag_close))

# Reshaping the data to have stock names as columns and values as the returns
dat_reshaped <- dat %>%
  dplyr::select(-close) %>%
  tidyr::spread(symbol, returns) %>%
  dplyr::select(-date)

# Finding the portfolio mix that minimizes risk for at least 0.05% return
model <- list()

# Constraint matrix
model$A <- matrix(c(rep(1, ncol(dat_reshaped)),
                    purrr::map_dbl(dat_reshaped, mean)),
                  nrow = 2,
                  byrow = TRUE)

# Objective function
model$Q <- cov(dat_reshaped)
model$obj <- c(0, 0, 0, 0, 0)

# RHS of Constraints
model$rhs <- c(1, 0.0005)

# Relational operator of the constraints
model$sense <- c("=", ">=")

# Finding the optimum solution
result <- gurobi::gurobi(model, list())
names(result$x) <- names(dat_reshaped)

# Optimal mix
paste0(formatC(result$x * 100, digits = 2, format = "f"), "%")

# Efficient Frontier -------------------

# Number of stocks
n_stocks <- ncol(dat_reshaped)

model <- list()

# Constraint matrix
model$A <- matrix(c(rep(1, n_stocks),
                    purrr::map_dbl(dat_reshaped, mean),
                    diag(n_stocks)
                    ),
                  byrow = TRUE,
                  ncol = n_stocks
                  )

# Objective function
model$Q <- cov(dat_reshaped)
model$obj <- c(0, 0, 0, 0, 0)

# Relational operator of the constraints
model$sense <- c("=", rep(">=", n_stocks + 1))

# Defining a grid of returns for which the corresponding minimum risk is to be computed
min_return <- seq(0.00001, 0.002, by = 0.00001)

# Initializing a list to store the risks, returns and the portfolio mix
efficient_frontier <- list()
efficient_frontier$return <- rep(NA_real_, length(min_return))
efficient_frontier$risk <- rep(NA_real_, length(min_return))
efficient_frontier$weights <- matrix(rep(NA_real_, length(min_return) * n_stocks),
                                     ncol = n_stocks)

# For each value of min_return, obtaining the corresponding minimized risk values
for (i in seq_along(min_return)) {
  model$rhs <- c(1, min_return[i], rep(0, n_stocks))
  result <- gurobi::gurobi(model, list())
  efficient_frontier$return[i] <- sum(result$x * purrr::map_dbl(dat_reshaped, mean))
  efficient_frontier$risk[i] <- sqrt(result$objval)
  efficient_frontier$weights[i, ] <- result$x
}

# Preparing the data to plot the efficient frontier
plot_dat <- dplyr::tibble(risk = efficient_frontier$risk,
                          return = efficient_frontier$return)

# Plotting the efficient frontier
ggplot2::ggplot(plot_dat, ggplot2::aes(x = risk, y = return)) +
  ggplot2::geom_line(group = 1, colour = "#FD625E") +
  ggplot2::scale_x_continuous(name = "Risk", labels = scales::percent_format(accuracy = 0.1)) +
  ggplot2::scale_y_continuous(name = "Expected Return", labels = scales::percent_format()) +
  ggplot2::theme_minimal(base_family = "tw_cen_mt") +
  ggplot2::theme(axis.line = ggplot2::element_line())

# Saving the plot
ggplot2::ggsave("efficient_frontier.png", device = "png")
# Setup ---------------------------

# Loading the libraries
library(tidyverse)
library(readxl)
library(lubridate)
library(scales)
library(nortest)
library(ks)

# Setting options for the R session
set.seed(12345)
n_sims <- 100000
options(tibble.width = Inf)
windowsFonts("tw_cen_mt" = windowsFont("Tw Cen MT"))
ggplot2::theme_set(
  ggplot2::theme_minimal(base_family = "tw_cen_mt")
)

# Defining a function to draw simulations from triangular distribution
rtri <- function(n, min, max, avg) {
  u <- runif(n)
  cut_off <- (avg - min) / (max - min)
  sim1 <- min + sqrt((max - min) * (avg - min) * u)
  sim2 <- max - sqrt((max - min) * (max - avg) * (1 - u))
  ifelse(u < cut_off, sim1, sim2)
}

# Drilling Costs -----------------------------

# Reading the data set - skipping first two rows from the Drilling Cost sheet,
# since it does not contain any data, making '.' to signify NA in the arithmetic
# change columns
dat <- readxl::read_xlsx("Analysis_Data.xlsx",
                         sheet = "Drilling Cost",
                         skip = 2,
                         na = ".")

# Changing the column names and extracting the year from date column
dat <- dat %>%
  purrr::set_names(c("Year", "Cost_CrudeOil", "Cost_NaturalGas", "Cost_DryWell",
                     "Return_Crude_Oil", "Return_NaturalGas",
                     "Return_DryWell")) %>%
  dplyr::mutate(Year = lubridate::year(Year))

avg_cost <- dat %>%
  dplyr::filter(Year == 2006) %>%
  dplyr::select(dplyr::starts_with("Cost")) %>%
  rowMeans()

# Taking all the arithmetic changes from 1991 to 2006 in a single column
changes <- dat %>%
  dplyr::filter(Year %in% 1991:2006) %>%
  dplyr::select(dplyr::starts_with("Return")) %>%
  purrr::reduce(c) %>%
  tibble::enframe(name = "id")

# 2006-2012 - Normal
mu <- mean(changes$value)
sig <- sd(changes$value)
sim_07_12_norm <- matrix(rnorm(n = 6 * n_sims, mean = mu, sd = sig),
                         ncol = 6,
                         nrow = n_sims,
                         byrow = FALSE)
colnames(sim_07_12_norm) <- c("2006-07", "2007-08", "2008-09", "2009-10", "2010-11", "2011-12")

# 2013-2015
sim_13_15 <- matrix(rtri(n = 3 * n_sims, min = -0.22, max = -0.07, avg = -0.0917),
                    ncol = 3,
                    nrow = n_sims,
                    byrow = FALSE)
colnames(sim_13_15) <- c("2012-13", "2013-14", "2014-15")

# 2016-2020
sim_16_20 <- matrix(rtri(n = 5 * n_sims, min = 0.02, max = 0.06, avg = 0.05),
                    ncol = 5,
                    nrow = n_sims,
                    byrow = FALSE)
colnames(sim_16_20) <- c("2015-16", "2016-17", "2017-18", "2018-19", "2019-20")

# Combining the simulations from 2007-2020
drilling_cost <- cbind(sim_07_12_norm, sim_13_15, sim_16_20)

# Determining the simulated costs in 2020
drilling_cost <- dplyr::tibble(value = avg_cost * apply(1 + drilling_cost, 1, prod))

# Plotting histogram of the simulations
ggplot2::ggplot(drilling_cost, ggplot2::aes(x = value)) +
  ggplot2::geom_histogram(fill = "#01B8AA", colour = "white") +
  ggplot2::geom_hline(yintercept = 0) +
  ggplot2::labs(x = "Average Cost in 2020", y = "Frequency") +
  ggplot2::scale_y_continuous(labels = scales::comma_format(), limits = c(NA, 20000), breaks = seq(0, 30000, by = 5000)) +
  ggplot2::scale_x_continuous(labels = scales::dollar_format()) +
  ggplot2::geom_vline(linetype = "dashed", data = NULL, mapping = ggplot2::aes(xintercept = avg_cost, colour = "avg_cost")) +
  ggplot2::scale_colour_manual(values = c("#FD625E"), name = "", labels = c("Average Cost in 2006")) +
  ggplot2::theme(legend.position = c(0.75, 0.75),
                 panel.grid.minor.y = ggplot2::element_blank())

# Writing the drilling cost simulations to a csv file
readr::write_csv(drilling_cost, "Drilling Cost Simulations.csv")
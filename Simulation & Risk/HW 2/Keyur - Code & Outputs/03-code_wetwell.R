# Setup --------------------------

# Loading the libraries
library(tidyverse)
library(readxl)
library(lubridate)
library(scales)
library(xlsx)

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

# Production Risk -----------------------

# Matrix of simulations from uniform and lognormal distributions
both_uni <- matrix(c(runif(n_sims, min = 0.15, 0.32),
                     rlnorm(n_sims, meanlog = 6, sdlog = 0.28)),
                   ncol = 2)

# Standardizing both the distributions
both_uni_std <- apply(both_uni, 2, scale)

# Correlation Matrix
sig <- matrix(c(1, 0.64, 0.64, 1), nrow = 2)

# Cholesky's decomposition
U <- t(chol(sig))

# Applying transformation to create correlated data, and de-standardizing it
both <- ((U %*% t(both_uni_std)) * apply(both_uni, 2, sd)) + colMeans(both_uni)
both <- t(both)

# Initial Production Rate for Year 1 (beginning)
init_prod <- both[, 2]

# Decline Rates
decline_rate <- matrix(rep(both[, 1], 15),
                       nrow = n_sims,
                       ncol = 15)
colnames(decline_rate) <- 2021:2035

# Computing the rates at the beginning of each year by cumulatively multiplying
# the initial rate with (1 - decline rate)
rates <- cbind("Initial Production Rate" = init_prod, 1 - decline_rate)
rates <- t(apply(rates, 1, cumprod))

# Rates at the beginning of each years from 2021 to 2035
rate_begin <- rates[, -ncol(rates)]
colnames(rate_begin) <- 2021:2035

# Rates at the end of each years from 2021 to 2035
rate_end <- rates[, -1]

# Computing the oil produced for the year using formula given in RFP
oil_volume_year <- 365 * ((rate_begin + rate_end) / 2)

# Revenue Risk -----------------------------

# Reading the data file that contains the oil price projects
dat <- readxl::read_xlsx("Analysis_Data.xlsx",
                         sheet = "Price Projections",
                         skip = 2) %>%
  dplyr::filter(Year %in% 2021:2035) %>%
  dplyr::select(`Low Oil Price`, `AEO2018 Reference`, `High Oil Price`) %>%
  purrr::set_names("Min", "Avg", "Max")

# Simulating the yearly price per barrel using a triangular distribution for each year
rev_per_barrel <- purrr::pmap(dat, ~rtri(n_sims, min = ..1, avg = ..2, max = ..3)) %>%
  purrr::set_names(2021:2035) %>%
  tibble::as_tibble() %>%
  as.matrix()

# Simulating Net Revenue Interest
nir <- matrix(rnorm(n_sims * 15, mean = 0.75, sd = 0.02),
              nrow = n_sims,
              dimnames = list(NULL, 2021:2035))

# Total Revenue after NIR
revenue <- oil_volume_year * rev_per_barrel * nir

# Operating Expenses --------------------------------------

# Simulating operating cost per barrel
unit_operating_cost <- matrix(rnorm(n_sims * 15, mean = 2.25, sd = 0.3),
                              nrow = n_sims,
                              dimnames = list(NULL, 2021:2035))

# Total Operating Cost
operating_cost <- oil_volume_year * unit_operating_cost

# Severance tax as a constant percentage of revenue
severance_tax <- revenue * 0.046

# Costs --------------------------------

# Reading the drilling costs simulated in Phase 1
drilling_cost <- readr::read_csv("Drilling Cost Simulations.csv") %>%
  dplyr::pull(value)

# Simulating the Seismic cost
seismic_cost <- rnorm(n = n_sims, mean = 600, sd = 50) * 960

# Simulating the Leasing cost
lease_cost <- rnorm(n = n_sims, mean = 3, sd = 0.35) * 43000

# Simulating the Completion Cost
completion_cost <- rnorm(n = n_sims, mean = 390000, sd = 50000)

# Professional Overhead for Year 0
professional_overhead <- rtri(n = n_sims, min = 172000, max = 279500, avg = 215000)

# Total cost in Year 0
cost_year0 <- seismic_cost + lease_cost + professional_overhead + drilling_cost + completion_cost

# Continued professional overhead cost - keeping it constant for next fifteen years
continued_cost <- matrix(rep(professional_overhead, 15),
                         nrow = n_sims,
                         dimnames = list(NULL, 2021:2035))

# Net Sales -------------------------------
net_sales <- revenue - operating_cost - severance_tax - continued_cost

# NPV ----------------------------------
wacc <- 0.1 # Weighted Average Cost of Captial
time_value <- 1 / ((1 + wacc) ^ (1:15)) # Time Value of unit money for 15 years
npv <- - cost_year0 + drop(net_sales %*% time_value) # Net Present Value

# Plotting the histogram of NPV
npv_df <- data.frame(value = npv)
xlsx::write.xlsx2(npv_df, "Simulated NPV of Wet Well.xlsx", sheetName = "Simulations", row.names = FALSE)

ggplot2::ggplot(npv_df, ggplot2::aes(x = value)) +
  ggplot2::geom_histogram(fill = "#01B8AA", colour = "white") +
  ggplot2::geom_hline(yintercept = 0) +
  ggplot2::labs(x = "Net Present Value of a Wet Well", y = "Frequency") +
  ggplot2::scale_y_continuous(labels = scales::comma_format()) +
  ggplot2::scale_x_continuous(labels = scales::dollar_format(scale = 10 ^ (-6), suffix = "M")) +
  ggplot2::theme(legend.position = c(0.75, 0.75),
                 panel.grid.minor.y = ggplot2::element_blank())
ggplot2::ggsave("Simulated NPV of Wet Well.png", device = "png")

# Calculating the descriptive statistics of NPV
funs <- c("Minimum" = min,
          "Maximum" = max,
          "5th Percentile" = function(x) quantile(x, probs = 0.05),
          "First Quartile" = function(x) quantile(x, probs = 0.25),
          "Median" = median,
          "Third Quartile" = function(x) quantile(x, probs = 0.75),
          "95th Percentile" = function(x) quantile(x, probs = 0.95),
          "Mean" = mean,
          "Standard Deviation" = sd)

purrr::map_dbl(funs, ~.x(npv)) %>%
  tibble::enframe(name = "Descriptive Statistic", value = "Net Present Value of Wet Well") %>%
  as.data.frame(stringsAsFactors = FALSE) %>%
  xlsx::write.xlsx2("Simulated NPV of Wet Well.xlsx", sheetName = "Descriptive Statistics", append = TRUE, row.names = FALSE)
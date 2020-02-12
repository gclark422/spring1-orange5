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

# 1. Since there is no production taking place, the only transactions taking
# place are costs in Year 0.
# 2. First one is Seismic Cost (Normal)
# 3. Second one is Lease Cost (Normal)
# 4. Third one is Professional Overhead Cost (Triangle)
# 5. Fourth one is Drilling Costs
drilling_cost <- readr::read_csv("Drilling Cost Simulations.csv") %>%
  dplyr::pull(value)
seismic_cost <- rnorm(n = n_sims, mean = 600, sd = 50) * 960
lease_cost <- rnorm(n = n_sims, mean = 3, sd = 0.35) * 43000
professional_overhead <- rtri(n = n_sims, min = 172000, max = 279500, avg = 215000)
tot_cost <- seismic_cost + lease_cost + professional_overhead + drilling_cost

dry_df <- data.frame(tot_cost)
xlsx::write.xlsx2(dry_df, "Simulated Cost of Dry Well.xlsx", sheetName = "Simulations", row.names = FALSE)

# Histogram plot of the simulations of dry well costs
ggplot2::ggplot(dry_df, ggplot2::aes(x = tot_cost)) +
  ggplot2::geom_histogram(colour = "white", fill = "#01B8AA") +
  ggplot2::geom_hline(yintercept = 0) +
  ggplot2::scale_x_continuous(labels = scales::dollar_format(scale = 10 ^ (-6), suffix = "M", accuracy = 0.1)) +
  ggplot2::scale_y_continuous(labels = scales::comma_format()) +
  ggplot2::labs(x = "Cost of Single Dry Well", y = "Frequency")
ggplot2::ggsave("Simulated Cost of Dry Well.png", device = "png")

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

purrr::map_dbl(funs, ~.x(tot_cost)) %>%
  tibble::enframe(name = "Descriptive Statistic", value = "Cost of Dry Well") %>%
  as.data.frame(stringsAsFactors = FALSE) %>%
  xlsx::write.xlsx2("Simulated Cost of Dry Well.xlsx", sheetName = "Descriptive Statistics", append = TRUE, row.names = FALSE)
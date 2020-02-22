# Loading the libraries -----------------
library(tidyverse)
library(lubridate)
library(readxl)
library(scales)
library(xlsx)

# Setting options for the R session --------------------
set.seed(12345)
n_sims <- 100000
options(tibble.width = Inf)
windowsFonts("tw_cen_mt" = windowsFont("Tw Cen MT"))
ggplot2::theme_set(
  ggplot2::theme_minimal(base_family = "tw_cen_mt")
)

# Defining a function to draw simulations from triangular distribution ----------------
rtri <- function(n, min, max, avg) {
  u <- runif(n)
  cut_off <- (avg - min) / (max - min)
  sim1 <- min + sqrt((max - min) * (avg - min) * u)
  sim2 <- max - sqrt((max - min) * (max - avg) * (1 - u))
  ifelse(u < cut_off, sim1, sim2)
}

# Defining a function to draw simulations from truncated normal distribution ----------------
rtrnorm <- function(n, mean = 0, sd = 1, a = -Inf, b = Inf) {
  u <- runif(n)
  u_trans <- (u * pnorm(b, mean, sd)) + ((1 - u) * pnorm(a, mean, sd))
  qnorm(u_trans, mean, sd)
}

# Defining a function to draw simulations for drilling costs ----------------
arith_changes  <- readxl::read_xlsx("data/in/Analysis_Data.xlsx",
                                    sheet = "Drilling Cost",
                                    skip = 2,
                                    na = ".") %>%
  purrr::set_names(c("Year", "Cost_CrudeOil", "Cost_NaturalGas", "Cost_DryWell",
                     "Return_Crude_Oil", "Return_NaturalGas",
                     "Return_DryWell")) %>%
  dplyr::mutate(Year = lubridate::year(Year))
avg_drilling_cost_2006 <- arith_changes %>%
  dplyr::filter(Year == 2006) %>%
  dplyr::select(dplyr::starts_with("Cost")) %>%
  rowMeans()
changes <- arith_changes %>%
  dplyr::filter(Year %in% 1991:2006) %>%
  dplyr::select(dplyr::starts_with("Return")) %>%
  purrr::reduce(c) %>%
  tibble::enframe(name = "id")
mean_arith_changes <- mean(changes$value)
sd_arith_changes <- sd(changes$value)
drill_cost <- function(n_sims) {
  sim_07_12_norm <- matrix(rnorm(n = 6 * n_sims, mean = mean_arith_changes, sd = sd_arith_changes),
                           ncol = 6,
                           nrow = n_sims,
                           byrow = FALSE)
  sim_13_15 <- matrix(rtri(n = 3 * n_sims, min = -0.22, max = -0.07, avg = -0.0917),
                      ncol = 3,
                      nrow = n_sims,
                      byrow = FALSE)
  sim_16_20 <- matrix(rtri(n = 5 * n_sims, min = 0.02, max = 0.06, avg = 0.05),
                      ncol = 5,
                      nrow = n_sims,
                      byrow = FALSE)
  drilling_cost <- cbind(sim_07_12_norm, sim_13_15, sim_16_20)
  avg_drilling_cost_2006 * apply(1 + drilling_cost, 1, prod) * 1000
}

# Defining a function to draw simulations for cost of a dry well ----------------
cost_dry_well <- function(n_sims) {
  if (n_sims == 0) {
    numeric(0)
  } else {
    drilling_cost <- drill_cost(n_sims)
    seismic_cost <- rnorm(n = n_sims, mean = 600, sd = 50) * 960
    lease_cost <- rnorm(n = n_sims, mean = 3, sd = 0.35) * 43000
    professional_overhead <- rtri(n = n_sims, min = 172000, max = 279500, avg = 215000)
    seismic_cost + lease_cost + professional_overhead + drilling_cost
  }
}

# Defining a function to draw simulations for NPV of a wet well ----------------
sig_production <- matrix(c(1, 0.64, 0.64, 1), nrow = 2)
U <- t(chol(sig_production))
price_proj <- readxl::read_xlsx("data/in/Analysis_Data.xlsx",
                                sheet = "Price Projections",
                                skip = 2) %>%
  dplyr::filter(Year %in% 2021:2035) %>%
  dplyr::select(`Low Oil Price`, `AEO2018 Reference`, `High Oil Price`) %>%
  purrr::set_names("Min", "Avg", "Max")
npv_wet_well <- function(n_sims) {
  if (n_sims == 0) {
    numeric(0)
  } else if (n_sims == 1) {
    npv_wet_well(2)[1]
  } else {
    both_uni <- matrix(c(runif(n_sims, min = 0.15, 0.32),
                         rlnorm(n_sims, meanlog = 6, sdlog = 0.28)),
                       ncol = 2)
    both_uni_std <- apply(both_uni, 2, scale)
    both <- ((U %*% t(both_uni_std)) * apply(both_uni, 2, sd)) + colMeans(both_uni)
    both <- t(both)
    init_prod <- both[, 2]
    decline_rate <- matrix(rep(both[, 1], 15),
                           nrow = n_sims,
                           ncol = 15)
    rates <- cbind("Initial Production Rate" = init_prod, 1 - decline_rate)
    rates <- t(apply(rates, 1, cumprod))
    rate_begin <- rates[, -ncol(rates)]
    rate_end <- rates[, -1]
    oil_volume_year <- 365 * ((rate_begin + rate_end) / 2)
    rev_per_barrel <- purrr::pmap(price_proj, ~rtri(n_sims, min = ..1, avg = ..2, max = ..3)) %>%
      purrr::set_names(2021:2035) %>%
      tibble::as_tibble() %>%
      as.matrix()
    nir <- matrix(rnorm(n_sims * 15, mean = 0.75, sd = 0.02),
                  nrow = n_sims,
                  dimnames = list(NULL, 2021:2035))
    revenue <- oil_volume_year * rev_per_barrel * nir
    unit_operating_cost <- matrix(rnorm(n_sims * 15, mean = 2.25, sd = 0.3),
                                  nrow = n_sims,
                                  dimnames = list(NULL, 2021:2035))
    operating_cost <- oil_volume_year * unit_operating_cost
    severance_tax <- revenue * 0.046
    drilling_cost <- drill_cost(n_sims)
    seismic_cost <- rnorm(n = n_sims, mean = 600, sd = 50) * 960
    lease_cost <- rnorm(n = n_sims, mean = 3, sd = 0.35) * 43000
    completion_cost <- rnorm(n = n_sims, mean = 390000, sd = 50000)
    professional_overhead <- rtri(n = n_sims, min = 172000, max = 279500, avg = 215000)
    cost_year0 <- seismic_cost + lease_cost + professional_overhead + drilling_cost + completion_cost
    continued_cost <- matrix(rep(professional_overhead, 15),
                             nrow = n_sims,
                             dimnames = list(NULL, 2021:2035))
    net_sales <- revenue - operating_cost - severance_tax - continued_cost
    wacc <- 0.1 # Weighted Average Cost of Captial
    time_value <- 1 / ((1 + wacc) ^ (1:15)) # Time Value of unit money for 15 years
    - cost_year0 + drop(net_sales %*% time_value) # Net Present Value
  }
}

# Simulations for probabilities of hydrocarbons and reservoir ----------------------------
p_h <- rtrnorm(n = n_sims, mean = 0.99, sd = 0.05, a = 0, b = 1)
p_h_df <- data.frame(value = p_h)
xlsx::write.xlsx2(p_h_df, "data/out/Simulated Probability of Hydrocarbons.xlsx", sheetName = "Simulations", row.names = FALSE)

ggplot2::ggplot(p_h_df, ggplot2::aes(x = value)) +
  ggplot2::geom_histogram(fill = "#01B8AA", colour = "white") +
  ggplot2::geom_hline(yintercept = 0) +
  ggplot2::labs(x = "Probability of Hydrocarbons", y = "Frequency") +
  ggplot2::scale_y_continuous(labels = scales::comma_format(), limits = c(NA, 12000), breaks = seq(0, 12000, by = 3000)) +
  ggplot2::scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  ggplot2::theme(legend.position = c(0.75, 0.75),
                 panel.grid.minor.y = ggplot2::element_blank())
ggplot2::ggsave("plots/Simulated Probability of Hydrocarbons.png", device = "png", width = 6.77, height = 3.78)

funs <- c("Minimum" = min,
          "Maximum" = max,
          "5th Percentile" = function(x) quantile(x, probs = 0.05),
          "First Quartile" = function(x) quantile(x, probs = 0.25),
          "Median" = median,
          "Third Quartile" = function(x) quantile(x, probs = 0.75),
          "95th Percentile" = function(x) quantile(x, probs = 0.95),
          "Mean" = mean,
          "Standard Deviation" = sd)

purrr::map_dbl(funs, ~.x(p_h)) %>%
  tibble::enframe(name = "Descriptive Statistic", value = "Probability of Hydrocarbons") %>%
  as.data.frame(stringsAsFactors = FALSE) %>%
  xlsx::write.xlsx2("data/out/Simulated Probability of Hydrocarbons.xlsx", sheetName = "Descriptive Statistics", append = TRUE, row.names = FALSE)

p_r <- rtrnorm(n = n_sims, mean = 0.8, sd = 0.1, a = 0, b = 1)
p_r_df <- data.frame(value = p_r)
xlsx::write.xlsx2(p_r_df, "data/out/Simulated Probability of Reservoir.xlsx", sheetName = "Simulations", row.names = FALSE)

ggplot2::ggplot(p_r_df, ggplot2::aes(x = value)) +
  ggplot2::geom_histogram(fill = "#01B8AA", colour = "white") +
  ggplot2::geom_hline(yintercept = 0) +
  ggplot2::labs(x = "Probability of Reservoir", y = "Frequency") +
  ggplot2::scale_y_continuous(labels = scales::comma_format(), limits = c(NA, 10000), breaks = seq(0, 10000, by = 2500)) +
  ggplot2::scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  ggplot2::theme(legend.position = c(0.75, 0.75),
                 panel.grid.minor.y = ggplot2::element_blank())
ggplot2::ggsave("plots/Simulated Probability of Resevoir.png", device = "png", width = 6.77, height = 3.78)

funs <- c("Minimum" = min,
          "Maximum" = max,
          "5th Percentile" = function(x) quantile(x, probs = 0.05),
          "First Quartile" = function(x) quantile(x, probs = 0.25),
          "Median" = median,
          "Third Quartile" = function(x) quantile(x, probs = 0.75),
          "95th Percentile" = function(x) quantile(x, probs = 0.95),
          "Mean" = mean,
          "Standard Deviation" = sd)

purrr::map_dbl(funs, ~.x(p_r)) %>%
  tibble::enframe(name = "Descriptive Statistic", value = "Probability of Reservoir") %>%
  as.data.frame(stringsAsFactors = FALSE) %>%
  xlsx::write.xlsx2("data/out/Simulated Probability of Reservoir.xlsx", sheetName = "Descriptive Statistics", append = TRUE, row.names = FALSE)

# Simulation of total project NPV ----------------------
# Defining a function to draw simulations for probability of producing well ----------------
rp <- function(n) {
  p_h <- rtrnorm(n, mean = 0.99, sd = 0.05, a = 0, b = 1) # Simulating probability of presence of hydrocarbons
  p_r <- rtrnorm(n, mean = 0.8, sd = 0.1, a = 0, b = 1) # Simulating probability of reservoir development in the rock formation
  rbernoulli(n, p = p_h * p_r)
}

# Simulating the number of wells from U(10, 30)
n_wells <- sample(x = 10:30, size = n_sims, replace = TRUE)

# For each each well in each simulation, deciding whether it is dry/wet using Bernoulli distribution
wet_or_dry <- purrr::map(n_wells, ~rp(n = .x))

# All 1's are wet wells
n_wet <- purrr::map_int(wet_or_dry, sum)
prop_wet <- n_wet / n_wells
prop_wet_df <- data.frame(value = prop_wet)
xlsx::write.xlsx2(prop_wet_df, "data/out/Simulated Proportion of Wet Wells.xlsx", sheetName = "Simulations", row.names = FALSE)

temp_df <- data.frame(x = quantile(prop_wet, 0.05),
                      y = 4500,
                      xend = 0.5,
                      yend = 7500)

ggplot2::ggplot(prop_wet_df, ggplot2::aes(x = value)) +
  ggplot2::geom_histogram(fill = "#01B8AA", colour = "white") +
  ggplot2::geom_hline(yintercept = 0) +
  ggplot2::labs(x = "Proportion of Wet Wells", y = "Frequency") +
  ggplot2::scale_y_continuous(labels = scales::comma_format(), limits = c(NA, 12000), breaks = seq(0, 12000, by = 3000)) +
  ggplot2::scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  ggplot2::theme(legend.position = c(0.75, 0.75),
                 panel.grid.minor.y = ggplot2::element_blank()) +
  ggplot2::geom_vline(xintercept = quantile(prop_wet, 0.05),
                      colour = "#F17825",
                      linetype = "dashed") +
  ggplot2::geom_curve(data = temp_df,
                      mapping = ggplot2::aes(x = x, xend = xend, y = y, yend = yend),
                      curvature = -0.5,
                      arrow = arrow(length = unit(0.25, "cm"), type = "closed"),
                      arrow.fill = "#F17825",
                      colour = "#F17825") +
  ggplot2::annotate("text", x = 0.5, y = 7500, label = bquote(atop(underline(bold("95% VaR:")), .(scales::percent(quantile(prop_wet, 0.05), accuracy = 0.1)))),
                    family = "tw_cen_mt",
                    vjust = -0.2,
                    colour = "grey30")
ggplot2::ggsave("plots/Simulated Proportion of Wet Wells.png", device = "png", width = 6.77, height = 3.78)

exp_shortfall <- function(x) {
  p5 <- quantile(x, probs = 0.05)
  mean(x[x <= p5])
}

funs <- c("Minimum" = min,
          "Maximum" = max,
          "5th Percentile" = function(x) quantile(x, probs = 0.05),
          "First Quartile" = function(x) quantile(x, probs = 0.25),
          "Median" = median,
          "Third Quartile" = function(x) quantile(x, probs = 0.75),
          "95th Percentile" = function(x) quantile(x, probs = 0.95),
          "Mean" = mean,
          "Standard Deviation" = sd,
          "Expected Shortfall" = exp_shortfall)

purrr::map_dbl(funs, ~.x(prop_wet)) %>%
  tibble::enframe(name = "Descriptive Statistic", value = "Proportion of Wet Wells") %>%
  as.data.frame(stringsAsFactors = FALSE) %>%
  xlsx::write.xlsx2("data/out/Simulated Proportion of Wet Wells.xlsx", sheetName = "Descriptive Statistics", append = TRUE, row.names = FALSE)

# For each wet well in each simulation, simulating the NPV
npv_wet <- purrr::map(n_wet, ~npv_wet_well(.x))
tot_npv <- purrr::map_dbl(npv_wet, sum)

# All 0's are dry wells. For each dry well in each simulation, simulating the cost
n_dry <- n_wells - n_wet
cost_dry <- purrr::map(n_dry, ~cost_dry_well(.x))
tot_cost <- purrr::map_dbl(cost_dry, sum)

# Project NPV Simulated Values
proj_npv <- tot_npv - tot_cost

proj_npv_df <- data.frame(value = proj_npv)
xlsx::write.xlsx2(proj_npv_df, "data/out/Simulated NPV of Project.xlsx", sheetName = "Simulations", row.names = FALSE)

# Calculating the descriptive statistics of NPV
funs <- c("Minimum" = min,
          "Maximum" = max,
          "5th Percentile" = function(x) quantile(x, probs = 0.05),
          "First Quartile" = function(x) quantile(x, probs = 0.25),
          "Median" = median,
          "Third Quartile" = function(x) quantile(x, probs = 0.75),
          "95th Percentile" = function(x) quantile(x, probs = 0.95),
          "Mean" = mean,
          "Standard Deviation" = sd,
          "Expected Shortfall" = exp_shortfall)

desc_stats <- purrr::map_dbl(funs, ~.x(proj_npv)) %>%
  tibble::enframe(name = "Descriptive Statistic", value = "Net Present Value of the Project") %>%
  as.data.frame(stringsAsFactors = FALSE)

xlsx::write.xlsx2(desc_stats, "data/out/Simulated NPV of Project.xlsx", sheetName = "Descriptive Statistics", append = TRUE, row.names = FALSE)

temp_df <- data.frame(x = c(quantile(proj_npv, 0.05), mean(proj_npv)),
                      y = c(8000, 4000),
                      xend = c(25 * 10 ^ 6, 350 * 10 ^ 6),
                      yend = c(6000, 6000),
                      stat = c("var", "mean"))

ggplot2::ggplot(proj_npv_df, ggplot2::aes(x = value)) +
  ggplot2::geom_histogram(fill = "#01B8AA", colour = "white", alpha = 0.85) +
  ggplot2::geom_hline(yintercept = 0) +
  ggplot2::labs(x = "Net Present Value of the Project", y = "Frequency") +
  ggplot2::scale_y_continuous(labels = scales::comma_format()) +
  ggplot2::scale_x_continuous(labels = scales::dollar_format(scale = 10 ^ (-6), suffix = "M")) +
  ggplot2::theme(legend.position = c(0.75, 0.75),
                 panel.grid.minor.y = ggplot2::element_blank()) +
  ggplot2::geom_vline(data = dplyr::filter(desc_stats, `Descriptive Statistic` %in% c("Mean", "5th Percentile")),
                      mapping = ggplot2::aes(xintercept = `Net Present Value of the Project`, colour = `Descriptive Statistic`),
                      linetype = "dashed",
                      show.legend = FALSE) +
  ggplot2::scale_colour_manual(values = c("#F17825", "#004753")) +
  ggplot2::geom_curve(data = dplyr::filter(temp_df, stat == "var"),
                      mapping = ggplot2::aes(x = x, xend = xend, y = y, yend = yend),
                      curvature = 0.25,
                      arrow = arrow(length = unit(0.25, "cm"), type = "closed"),
                      colour = "#F17825",
                      arrow.fill = "#F17825") +
  ggplot2::geom_curve(data = dplyr::filter(temp_df, stat == "mean"),
                      mapping = ggplot2::aes(x = x, xend = xend, y = y, yend = yend),
                      curvature = 0.25,
                      arrow = arrow(length = unit(0.25, "cm"), type = "closed"),
                      colour = "#004753",
                      arrow.fill = "#004753") +
  ggplot2::annotate("text", x = 25 * 10 ^ 6, y = 6000,
                    label = bquote(atop(underline(bold("95% VaR:")), .(scales::dollar(quantile(proj_npv, 0.05), accuracy = 0.1, suffix = "M", scale = 10 ^ -6)))),
                    family = "tw_cen_mt", colour = "grey30", vjust = 1.2) +
  ggplot2::annotate("text", x = 350 * 10 ^ 6, y = 6000,
                    label = bquote(atop(underline(bold("Expected Return:")), .(scales::dollar(mean(proj_npv), accuracy = 0.1, suffix = "M", scale = 10 ^ -6)))),
                    family = "tw_cen_mt", colour = "grey30", vjust = -0.2)
ggplot2::ggsave("plots/Simulated NPV of Project.png", device = "png", width = 7.5, height = 3.78)
# Loading libraries
library(tidyverse)
library(haven)
library(smbinning)
library(ggthemes)

# setwd("C:\\Users\\Keyur\\Google Drive\\NCSU\\AA503\\spring1-orange5")

# Setting options for the R Session
options(tibble.width = Inf)
windowsFonts("tw_cen_mt" = windowsFont("Tw Cen MT"))

# Reading the accepts and rejects data sets
accepts <- haven::read_sas("Financial Analytics/accepted_customers.sas7bdat")
rejects <- haven::read_sas("Financial Analytics/rejected_customers.sas7bdat")

# 1. Removing "," from PRODUCT and PROF since smbinning is giving an error
# 2. Replacing "" with missing values in PRODUCT, RESID and PROF columns
# 3. Converting various columns into factors
accepts <- accepts %>%
  dplyr::mutate(PRODUCT = gsub(",", "", PRODUCT),
                PROF = gsub(",", "", PROF)) %>%
  dplyr::mutate_at(c("PRODUCT", "RESID", "PROF"),
                   ~ifelse(is.na(.x), NA, .x)) %>%
  dplyr::mutate_at(c("BUREAU", "CAR", "CARDS", "DIV", "EC_CARD", "FINLOAN",
                     "LOCATION", "NAT", "PRODUCT", "PROF", "REGN", "RESID",
                     "STATUS", "TEL", "TITLE", "NMBLOAN"),
                   factor) %>%
  dplyr::mutate(good = ifelse(GB == 1, 0L, 1L),
                bad = ifelse(GB == 1, 1L, 0L)) %>%
  dplyr::mutate(GB = as.integer(GB)) %>%
  dplyr::select(-STATUS) %>%
  as.data.frame()

# 1. Removing "," from PRODUCT and PROF since smbinning is giving an error
# 2. Replacing "" with missing values in PRODUCT, RESID and PROF columns
# 3. Converting various columns into factors
rejects <- rejects %>%
  dplyr::mutate(PRODUCT = gsub(",", "", PRODUCT),
                PROF = gsub(",", "", PROF)) %>%
  dplyr::mutate_at(c("PRODUCT", "RESID"), ~ifelse(is.na(.x), NA, .x)) %>%
  dplyr::mutate_at(c("BUREAU", "CAR", "CARDS", "DIV", "EC_CARD", "FINLOAN",
                     "LOCATION", "NAT", "PRODUCT", "PROF", "REGN", "RESID",
                     "STATUS", "TEL", "TITLE", "NMBLOAN"),
                   factor) %>%
  dplyr::select(-STATUS) %>%
  as.data.frame()

# Setting the seed to partition the accepts data set
set.seed(12345)

# Taking a sample of size 70% for training data set
train_id <- sample(seq_len(nrow(accepts)), size = floor(0.7 * nrow(accepts)))

# Filtering the IDs chosen above for training set
train <- accepts[train_id, ]

# Filtering out the IDs chosen above for training set
test <- accepts[-train_id, ]

# Information value of all variables in the data set
iv_summary <- smbinning::smbinning.sumiv(df = train, y = "good") %>%
  dplyr::mutate(strength = ifelse(IV >= 0.3, "Strong",
                                  ifelse(IV >= 0.1, "Medium",
                                         "Weak")),
                strength = factor(strength, levels = c("Strong", "Medium", "Weak")))
# Plotting a dotchart of the information value in descending order and coloring
# by the strength of the variable (Strong = >=0.3, Medium = >= 0.1, Weak = Otherwise)
iv_summary %>%
  dplyr::filter(!is.na(IV)) %>%
  dplyr::arrange(IV) %>%
  dplyr::mutate(Char = factor(Char, levels = Char, ordered = TRUE)) %>%
  ggplot2::ggplot(ggplot2::aes(x = Char, y = IV, colour = strength)) +
  ggplot2::geom_point(size = 3) +
  ggplot2::coord_flip() +
  ggplot2::theme_minimal(base_family = "tw_cen_mt") +
  ggplot2::labs(x = "Variable", y = "Information Value", colour = "Strength") +
  ggplot2::theme(
    legend.position = "bottom",
    axis.title.y = element_blank()
  ) +
  ggplot2::scale_colour_brewer(type = "seq", palette = "Reds", direction = -1) +
  ggplot2::geom_hline(yintercept = 0.3, linetype = "dashed", alpha = 0.5) +
  ggplot2::geom_hline(yintercept = 0.1, linetype = "dashed", alpha = 0.5)
ggplot2::ggsave("Financial Analytics/info_val_dotplot.png", device = "png")


# Binning the variables having IV >= 0.1 and including these binned variables
# in the data set
variables <- iv_summary %>%
  dplyr::filter(IV >= 0.1) %>%
  dplyr::pull(Char) %>%
  as.character()
variables_classes <- purrr::map_chr(train, class)[variables]
names(variables) <- variables
results <- purrr::map2(variables,
                       variables_classes,
                       ~if (.y == "factor") {
                         smbinning::smbinning.factor(df = train, y = "GB", x = .x)
                       } else {
                         smbinning::smbinning(df = train, y = "GB", x = .x)
                       })

# Function to include the binned variable in the data set
f <- function(x, y) {
  tryCatch(
    smbinning::smbinning.gen(df = x, ivout = y, chrname = paste0(y[["x"]], "_bin")),
    error = function(e) smbinning::smbinning.factor.gen(df = x, ivout = y, chrname = paste0(y[["x"]], "_bin"))
  )
}

# Iteratively applying the function 'f' to all the significant variables
train <- purrr::reduce(c(list(train), results), f)

# Introducing the WoE values of the binned variables to the data set
for (j in seq_along(results)) {
  for (i in seq_len(nrow(train))) {
    bin_name <- paste(results[[j]]$x, "_bin", sep = "")
    bin <- substr(train[[bin_name]][i], 2, 2)

    woe_name <- paste(results[[j]]$x, "_WOE", sep = "")

    if (bin == 0) {
      bin <- dim(results[[j]]$ivtable)[1] - 1
      train[[woe_name]][i] <- results[[j]]$ivtable[bin, "WoE"]
    } else {
      train[[woe_name]][i] <- results[[j]]$ivtable[bin, "WoE"]
    }
  }
}

# initial_score <- glm(data = train, BAD ~ AGE_WOE +
#                                          TMJOB1_WOE +
#                                          INCOME_WOE +
#                                          PERS_H_WOE +
#                                          CARDS_WOE +
#                                          EC_CARD_WOE,
#                      weights = train$`_freq_`, family = "binomial")

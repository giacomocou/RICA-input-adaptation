### Script for WHEAT - Monthly specification ###################################

# The script performs the following steps:
# 1. Loads and prepares the dataset for wheat, including cleaning and creating new variables.
# 2. Joins weather data (monthly temperatures and precipitation by phenological phase) to the farm-level dataset.
# 3. Joins input and output price data to the dataset.
# 4. Computes descriptive statistics for key variables.
# 5. Estimates a reduced-form regression of yield on phenological phase weather variables.

# ---- preliminary stuff -------------------------------------------------------
rm(list=ls())
gc()

# load packages
wants <- c("tidyverse", "readr", "lfe", "readxl",
           "DescTools", "conleyreg", "fixest")
needs <- wants[!(wants %in% installed.packages()[,"Package"])]
if(length(needs)) install.packages(needs)
lapply(wants, function(i) require(i, character.only=TRUE))
rm(needs,wants)

# ---- load data ---------------------------------------------------------------

d <- read.csv("Data/Raw data/RICA/synthetic_wheat.csv")
d$mun_code <- as.character(d$mun_code)

# ---- cleaning and preparing data ---------------------------------------------

# create per area variables
d <- d %>%
  mutate(
    yield = quantity/area,
    fertiliser_per_area = fertiliser/area,
    pesticide_per_area = pesticide/area,
    ln_yield = log(yield)
  )

# ---- check outliers ----------------------------------------------------------

# boxplot of yield over regions for visual inspection
ggplot(d, aes(x = factor(region), y = yield)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Distribution of Yield Over Regions", x = "Region", y = "Yield per ha") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

# boxplot of fertiliser over regions for visual inspection
ggplot(d, aes(x = factor(region), y = fertiliser_per_area)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Distribution of fertiliser per ha Over Regions", x = "Region", y = "Fertiliser per ha") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

# winsorize variables with outliers
d <- d %>%
  mutate(across(c(yield, fertiliser_per_area, pesticide_per_area), ~ Winsorize(., val = quantile(., probs = c(0.01, 0.99), na.rm = TRUE))))

# final number of farms
length(unique(d$farm_code))

# ---- join weather data -------------------------------------------------------

# load monthly temperatures
temps <- readRDS("Data/Raw data/Temperature/Wheat monthly temperatures RICA unbiased.rds")
temps <- temps %>% ungroup()
temps$code <- as.character(temps$code)
temps <- temps %>% rename(mun_code = code,
                          year = data_anno)
temps[, 3:14] <- round(temps[, 3:14], 2)

# assign oct, nov and dec to the successive growing season
temps <- temps %>%
  pivot_longer(cols = starts_with("month"),
               names_to = "month",
               names_prefix = "month",
               values_to = "temperature")

temps <- temps %>%
  mutate(
    year = case_when(
      month %in% 1:9 ~ year,
      month %in% 10:12 ~ year + 1,
      TRUE ~ NA_real_
    )
  )

temps <- temps %>%
  pivot_wider(names_from = month,
              values_from = temperature,
              names_prefix = "month")

# compute mean temperature by phenological phase
# autumn: October to November
# winter: December to February
# spring: March to April
# summer: May to June
temps <- temps %>%
  mutate(temp_autumn = round(rowMeans(select(., c(month10, month11)), na.rm = TRUE), 2),
         temp_win = round(rowMeans(select(., c(month12, month1, month2)), na.rm = TRUE), 2),
         temp_spring = round(rowMeans(select(., c(month3, month4)), na.rm = TRUE), 2),
         temp_summer = round(rowMeans(select(., c(month5, month6)), na.rm = TRUE), 2)
  ) %>%
  select(mun_code, year, temp_autumn, temp_win, temp_spring, temp_summer)

# load monthly precipitation
prec <- read_csv("Data/Raw data/Precipitation/Wheat monthly precipitation.csv")
prec$mun_code <- as.character(prec$mun_code)

# assign oct, nov and dec to the successive growing season
prec <- prec %>%
  pivot_longer(cols = starts_with("prec"),
               names_to = "month",
               names_prefix = "prec",
               values_to = "precipitation")

prec <- prec %>%
  mutate(
    year = case_when(
      month %in% 1:9 ~ year,
      month %in% 10:12 ~ year + 1,
      TRUE ~ NA_real_
    )
  )

prec <- prec %>%
  pivot_wider(names_from = month,
              values_from = precipitation,
              names_prefix = "month")

# compute total precipitation by phenological phase
prec <- prec %>%
  mutate(prec_autumn = round(rowSums(select(., c(month10, month11)), na.rm = TRUE), 2),
         prec_win = round(rowSums(select(., c(month12, month1, month2)), na.rm = TRUE), 2),
         prec_spring = round(rowSums(select(., c(month3, month4)), na.rm = TRUE), 2),
         prec_summer = round(rowSums(select(., c(month5, month6)), na.rm = TRUE), 2)
  ) %>%
  select(mun_code, year, prec_autumn, prec_win, prec_spring, prec_summer)

# join temperatures and precipitation
d <- left_join(d, temps, by = c("mun_code", "year"))
d <- left_join(d, prec, by = c("mun_code", "year"))

# ---- join input prices -------------------------------------------------------

# import input price data
input_prices <- read.csv("Data/Raw data/ISTAT/Input prices.csv", sep = ";")
input_prices <- input_prices %>%
  rename(price_fertiliser = difesa,
         price_pesticide = concimi
  ) %>%
  mutate(price_fertiliser = price_fertiliser/100, # rescale from base 100 to base 1
         price_pesticide = price_pesticide/100
  )

d <- left_join(d, input_prices, by = "year")

# create deflated variables
d <- d %>%
  mutate(fertiliser_per_area_deflated = fertiliser_per_area/price_fertiliser,
         pesticide_per_area_deflated = pesticide_per_area/price_pesticide,
  )

# ---- descriptive statistics --------------------------------------------------

descriptive_stats <- d %>%
  select(temp_autumn, temp_win, temp_spring, temp_summer, prec_autumn, prec_win, prec_spring, prec_summer, yield, area, price_fertiliser, price_pesticide, fertiliser_per_area_deflated, pesticide_per_area_deflated) %>%
  reframe(
    Mean = sapply(., function(x) round(mean(x, na.rm = TRUE), 3)),
    SD = sapply(., function(x) round(sd(x, na.rm = TRUE), 3)),
    Min = sapply(., function(x) round(min(x, na.rm = TRUE), 3)),
    Max = sapply(., function(x) round(max(x, na.rm = TRUE), 3))
  )

descriptive_stats$Variable <- c("Temperature - Autumn (Oct-Nov)", "Temperature - Winter (Dec-Feb)", "Temperature - Spring (Mar-Apr)", "Temperature - Summer (May-Jun)",
                                "Precipitation - Autumn (Oct-Nov)", "Precipitation - Winter (Dec-Feb)", "Precipitation - Spring (Mar-Apr)", "Precipitation - Summer (May-Jun)",
                                "Yield", "Area", "Fertilizer price (2010=1)", "Pesticide price (2010=1)",
                                "Fertilizer applications per Ha", "Pesticide applications per Ha")

descriptive_stats <- descriptive_stats %>%
  select(Variable, Mean, SD, Min, Max)

################################################################################
# ANALYSIS #####################################################################
################################################################################

# avoid results in scientific notation
options(scipen = 999)

# ---- reduced form ------------------------------------------------------------

model_Y_fixest <- feols(yield ~ temp_autumn + temp_win + temp_spring + temp_summer + prec_autumn + prec_win + prec_spring + prec_summer | farm_code, data = d)
summary(model_Y_fixest) # clustered SEs by farm_code (fixest default)
summary(model_Y_fixest, vcov = "hetero")
d$agr_reg_year <- interaction(d$agr_reg_code, d$year, drop = TRUE)
summary(model_Y_fixest, vcov = ~agr_reg_year)

# conley SEs
vcov_reduced_conley <- conleyreg(
  model_Y_fixest,
  data = d,
  unit = "farm_code",
  dist_cutoff = 100,
  vcov = TRUE,
  lat = "latitude",
  lon = "longitude"
)

summary(model_Y_fixest, vcov = vcov_reduced_conley)

# ---- end of script -----------------------------------------------------------
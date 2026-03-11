### Script for CORN - Monthly specification ####################################

# The script performs the following steps:
# 1. Loads and prepares the dataset for corn, including cleaning and creating new variables.
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

d <- read.csv("Data/Raw data/RICA/synthetic_corn.csv")
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
temps <- readRDS("Data/Raw data/Temperature/Corn monthly temperatures RICA unbiased.rds")
temps <- temps %>% ungroup()
temps$code <- as.character(temps$code)
temps <- temps %>% rename(mun_code = code,
                          year = data_anno)
temps[, 3:14] <- round(temps[, 3:14], 2)

# compute mean temperature by phenological phase
# germination to flowering: March to May
# flowering to maturity: June to August
# harvest: September
temps <- temps %>%
  mutate(temp_germ = round(rowMeans(select(., month3:month5), na.rm = TRUE), 2),
         temp_flow = round(rowMeans(select(., month6:month8), na.rm = TRUE), 2),
         temp_mat = month9
  ) %>%
  select(mun_code, year, temp_germ, temp_flow, temp_mat)

# load monthly precipitation
prec <- read_csv("Data/Raw data/Precipitation/Corn monthly precipitation.csv")
prec$mun_code <- as.character(prec$mun_code)

# compute total precipitation by phenological phase
prec <- prec %>%
  mutate(prec_germ = rowSums(select(., prec3:prec5), na.rm = TRUE),
         prec_flow = rowSums(select(., prec6:prec8), na.rm = TRUE),
         prec_mat = prec9
  ) %>%
  select(mun_code, year, prec_germ, prec_flow, prec_mat)

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
  select(temp_germ, temp_flow, temp_mat, prec_germ, prec_flow, prec_mat, yield, area, price_fertiliser, price_pesticide, fertiliser_per_area_deflated, pesticide_per_area_deflated) %>%
  reframe(
    Mean = sapply(., function(x) round(mean(x, na.rm = TRUE), 3)),
    SD = sapply(., function(x) round(sd(x, na.rm = TRUE), 3)),
    Min = sapply(., function(x) round(min(x, na.rm = TRUE), 3)),
    Max = sapply(., function(x) round(max(x, na.rm = TRUE), 3))
  )

descriptive_stats$Variable <- c("Temperature - Germination (Mar-May)", "Temperature - Flowering (Jun-Aug)", "Temperature - Maturity (Sep)",
                                "Precipitation - Germination (Mar-May)", "Precipitation - Flowering (Jun-Aug)", "Precipitation - Maturity (Sep)",
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

model_Y_fixest <- feols(yield ~ temp_germ + temp_flow + temp_mat + prec_germ + prec_flow + prec_mat | farm_code, data = d)
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
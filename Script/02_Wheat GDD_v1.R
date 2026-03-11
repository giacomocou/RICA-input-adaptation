### Script for WHEAT - GDD specification #######################################

# The script performs the following steps:
# 1. Loads and prepares the dataset for wheat, including cleaning and creating new variables.
# 2. Joins weather data (GDD, KDD and precipitation) to the farm-level dataset.
# 3. Joins input and output price data to the dataset.
# 4. Estimates a reduced-form regression of yield on weather variables.
# 5. Estimates a structural model using Seemingly Unrelated Regression (SUR).
# 6. Compares reduced-form and structural elasticities at the mean KDD.

# ---- preliminary stuff -------------------------------------------------------
rm(list=ls())
gc()

# load packages
wants <- c("tidyverse", "readr", "lfe", "readxl", "RColorBrewer", "systemfit",
           "DescTools", "rnaturalearth", "rnaturalearthdata", "sf", "conleyreg", "fixest")
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

# load GDD and KDD
temps <- readRDS("Data/Raw data/Temperature/Wheat_GDD_KDD_hist.rds")
temps[, 3:4] <- round(temps[, 3:4], 4)

# load monthly precipitation
prec <- read_csv("Data/Raw data/Precipitation/Wheat monthly precipitation.csv")
prec$mun_code <- as.character(prec$mun_code)

# assign oct, nov and dec to the successive growing season
prec <- prec %>%
  pivot_longer(cols = starts_with("prec"),
               names_to = "month",
               names_prefix = "prec",
               values_to = "precipitation")

prec$month <- as.numeric(prec$month)

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

# compute total precipitation during the growing season
prec <- prec %>%
  mutate(sum_prec = rowSums(select(., c(month1, month2, month3, month4,
                                        month5, month6, month7, month10,
                                        month11, month12)), na.rm = TRUE)) %>%
  select(mun_code, year, sum_prec)

# join weather variables
d <- left_join(d, temps, by = c("mun_code", "year"))
d <- left_join(d, prec, by = c("mun_code", "year"))

# create square of precipitation
d$sum_prec_2 = d$sum_prec*d$sum_prec

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

# ---- join output prices ------------------------------------------------------

# import output price data
output_prices <- read.csv("Data/Raw data/ISTAT/Output prices.csv", sep = ";")
output_prices <- output_prices %>%
  rename(year = anno,
         price_wheat = price_tenero) %>%
  select(year, price_wheat)

d <- left_join(d, output_prices, by = "year")

# compute relative prices
d <- d %>%
  mutate(rel_price_n = price_fertiliser/price_wheat,
         rel_price_ph = price_pesticide/price_wheat,
         rel_price_n2 = rel_price_n*rel_price_n,
         rel_price_ph2 = rel_price_ph*rel_price_ph,
         rel_price_nph = rel_price_n*rel_price_ph)

################################################################################
# ANALYSIS #####################################################################
################################################################################

# ---- setup -------------------------------------------------------------------

# within variables of all the interactions
d <- d %>%
  group_by(farm_code) %>%
  mutate(
    yield_within = yield - mean(yield),
    ln_yield_within = ln_yield - mean(ln_yield),
    gdd_within = gdd - mean(gdd),
    kdd_within = kdd - mean(kdd),
    sum_prec_within = sum_prec - mean(sum_prec),
    sum_prec_2_within = sum_prec_2 - mean(sum_prec_2),
    x_n_within = fertiliser_per_area_deflated - mean(fertiliser_per_area_deflated),
    x_ph_within = pesticide_per_area_deflated - mean(pesticide_per_area_deflated),
    rel_price_n_within = rel_price_n - mean(rel_price_n),
    rel_price_n_gdd_within = rel_price_n*gdd - mean(rel_price_n*gdd),
    rel_price_n_kdd_within = rel_price_n*kdd - mean(rel_price_n*kdd),
    rel_price_n_sum_prec_within = rel_price_n*sum_prec - mean(rel_price_n*sum_prec),
    rel_price_n_sum_prec_2_within = rel_price_n*sum_prec_2 - mean(rel_price_n*sum_prec_2),
    rel_price_n2_within = rel_price_n2 - mean(rel_price_n2),
    rel_price_n2_gdd_within = rel_price_n2*gdd - mean(rel_price_n2*gdd),
    rel_price_n2_kdd_within = rel_price_n2*kdd - mean(rel_price_n2*kdd),
    rel_price_n2_sum_prec_within = rel_price_n2*sum_prec - mean(rel_price_n2*sum_prec),
    rel_price_n2_sum_prec_2_within = rel_price_n2*sum_prec_2 - mean(rel_price_n2*sum_prec_2),
    rel_price_ph_within = rel_price_ph - mean(rel_price_ph),
    rel_price_ph_gdd_within = rel_price_ph*gdd - mean(rel_price_ph*gdd),
    rel_price_ph_kdd_within = rel_price_ph*kdd - mean(rel_price_ph*kdd),
    rel_price_ph_sum_prec_within = rel_price_ph*sum_prec - mean(rel_price_ph*sum_prec),
    rel_price_ph_sum_prec_2_within = rel_price_ph*sum_prec_2 - mean(rel_price_ph*sum_prec_2),
    rel_price_ph2_within = rel_price_ph2 - mean(rel_price_ph2),
    rel_price_ph2_gdd_within = rel_price_ph2*gdd - mean(rel_price_ph2*gdd),
    rel_price_ph2_kdd_within = rel_price_ph2*kdd - mean(rel_price_ph2*kdd),
    rel_price_ph2_sum_prec_within = rel_price_ph2*sum_prec - mean(rel_price_ph2*sum_prec),
    rel_price_ph2_sum_prec_2_within = rel_price_ph2*sum_prec_2 - mean(rel_price_ph2*sum_prec_2),
    rel_price_nph_within = rel_price_nph - mean(rel_price_nph),
    rel_price_nph_gdd_within = rel_price_nph*gdd - mean(rel_price_nph*gdd),
    rel_price_nph_kdd_within = rel_price_nph*kdd - mean(rel_price_nph*kdd),
    rel_price_nph_sum_prec_within = rel_price_nph*sum_prec - mean(rel_price_nph*sum_prec),
    rel_price_nph_sum_prec_2_within = rel_price_nph*sum_prec_2 - mean(rel_price_nph*sum_prec_2)
  ) %>%
  ungroup()

# avoid results in scientific notation
options(scipen = 999)


# ---- reduced form ------------------------------------------------------------

model_Y_fixest <- feols(yield ~ gdd + kdd + sum_prec + sum_prec_2 | farm_code, data = d)
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

# ---- structural model --------------------------------------------------------

# define formulas for the three equations of the system
yield_within.formula <- yield_within ~
  a_gdd*gdd_within + a_kdd*kdd_within +
  a_prec*sum_prec_within + a_prec2*sum_prec_2_within -
  .5*(d11_0*rel_price_ph2_within + d11_gdd*rel_price_ph2_gdd_within +
        d11_kdd*rel_price_ph2_kdd_within + d11_prec*rel_price_ph2_sum_prec_within +
        d11_prec2*rel_price_ph2_sum_prec_2_within) -
  .5*(d22_0*rel_price_n2_within + d22_gdd*rel_price_n2_gdd_within +
        d22_kdd*rel_price_n2_kdd_within + d22_prec*rel_price_n2_sum_prec_within +
        d22_prec2*rel_price_n2_sum_prec_2_within) +
  (d12_0*rel_price_nph_within + d12_gdd*rel_price_nph_gdd_within +
     d12_kdd*rel_price_nph_kdd_within + d12_prec*rel_price_nph_sum_prec_within +
     d12_prec2*rel_price_nph_sum_prec_2_within)

x_n_within.formula <- x_n_within ~
  b1_gdd*gdd_within + b1_kdd*kdd_within +
  b1_prec*sum_prec_within + b1_prec2*sum_prec_2_within -
  (d22_0*rel_price_n_within + d22_gdd*rel_price_n_gdd_within +
     d22_kdd*rel_price_n_kdd_within + d22_prec*rel_price_n_sum_prec_within +
     d22_prec2*rel_price_n_sum_prec_2_within) +
  (d12_0*rel_price_ph_within + d12_gdd*rel_price_ph_gdd_within +
     d12_kdd*rel_price_ph_kdd_within + d12_prec*rel_price_ph_sum_prec_within +
     d12_prec2*rel_price_ph_sum_prec_2_within)

x_ph_within.formula <- x_ph_within ~
  b2_gdd*gdd_within + b2_kdd*kdd_within +
  b2_prec*sum_prec_within + b2_prec2*sum_prec_2_within -
  (d11_0*rel_price_ph_within + d11_gdd*rel_price_ph_gdd_within +
     d11_kdd*rel_price_ph_kdd_within + d11_prec*rel_price_ph_sum_prec_within +
     d11_prec2*rel_price_ph_sum_prec_2_within) +
  (d12_0*rel_price_n_within + d12_gdd*rel_price_n_gdd_within +
     d12_kdd*rel_price_n_kdd_within + d12_prec*rel_price_n_sum_prec_within +
     d12_prec2*rel_price_n_sum_prec_2_within)

# define starting values for convergence
start.values <- c(
  # α
  a_gdd=100, a_kdd=-1, a_prec=-1, a_prec2=-1,
  # δ_(2,2)
  d22_0=-1000, d22_gdd=1000, d22_kdd=-100, d22_prec=-1, d22_prec2=1,
  # δ_(1,1)
  d11_0=1000, d11_gdd=500, d11_kdd=-10, d11_prec=-10, d11_prec2=1,
  # δ_(1,2)
  d12_0=-1000, d12_gdd=100, d12_kdd=-10, d12_prec=-1, d12_prec2=1,
  # β_1
  b1_gdd=-10, b1_kdd=1, b1_prec=1, b1_prec2=0.01,
  # β_2
  b2_gdd=10, b2_kdd=-1, b2_prec=-1, b2_prec2=0.1
)

if(F){ # safeguard since this takes a while to run
  model_within <- list(yield_within.formula, x_n_within.formula, x_ph_within.formula)
  structural_model <- nlsystemfit("SUR", model_within, start.values, data=d)
  saveRDS(structural_model, "Output/Wheat/Wheat_GDD_structural_model.RDS")
}

# use already saved model
structural_model <- readRDS("Output/Wheat/Wheat_GDD_structural_model.RDS")

# extract estimates
coefficients <- structural_model$b
standard_errors <- structural_model$se
t_values <- structural_model$t
p_values <- structural_model$p

summary_table <- data.frame(
  Coefficient = coefficients,
  SE = standard_errors,
  t_value = t_values,
  p_value = p_values
)
print(summary_table)

#write.csv(summary_table, "Output/Wheat/Wheat_summary_structural_GDD.csv", row.names= T, quote=FALSE)


################################################################################
# ELASTICITIES #################################################################
################################################################################

# ---- reduced form elasticity at the mean KDD ---------------------------------

beta_rd <- model_Y_fixest %>% coef()
vcov_rd <- vcov(model_Y_fixest)

# KDD elasticity
el_reduced_kdd <- (beta_rd["kdd"] * mean(d$kdd)) / mean(d$yield)
el_reduced_kdd_var <- (t(c(0, 0, mean(d$kdd), 0, 0)) %*% vcov_rd %*% c(0, 0, mean(d$kdd), 0, 0))
el_reduced_kdd_sd <- (1/mean(d$yield)) * (el_reduced_kdd_var)^0.5

# ---- structural model elasticity at the mean KDD ----------------------------

mean_gdd <- 0
mean_kdd <- mean(d$kdd)

me_yield_st <- (structural_model$b[1]*mean_gdd + structural_model$b[2]*mean_kdd -
  0.5*mean(d$rel_price_n)^2*(structural_model$b[6]*mean_gdd + structural_model$b[7]*mean_kdd) -
  0.5*mean(d$rel_price_ph)^2*(structural_model$b[11]*mean_gdd + structural_model$b[12]*mean_kdd) +
  mean(d$rel_price_n)*mean(d$rel_price_ph)*(structural_model$b[16]*mean_gdd + structural_model$b[17]*mean_kdd)) / mean(d$yield)

gradient_st <- c(mean_gdd, mean_kdd, 0, 0,
                 0, -0.5*mean(d$rel_price_n)^2*mean_gdd, -mean(d$rel_price_n)^2*mean_kdd, 0, 0,
                 0, -0.5*mean(d$rel_price_ph)^2*mean_gdd, -mean(d$rel_price_ph)^2*mean_kdd, 0, 0,
                 0, mean(d$rel_price_ph)*mean(d$rel_price_n)*mean_gdd, mean(d$rel_price_ph)*mean(d$rel_price_n)*mean_kdd, 0, 0,
                 0, 0, 0, 0,
                 0, 0, 0, 0)

me_yield_st_var <- t(gradient_st) %*% structural_model$covb %*% gradient_st
me_yield_st_sd <- (me_yield_st_var^0.5) / mean(d$yield)

# ---- end of script -----------------------------------------------------------
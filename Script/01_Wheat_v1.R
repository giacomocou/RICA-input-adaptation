### Script for WHEAT ###########################################################

# The script performs the following steps:
# 1. Loads and prepares the dataset for wheat, including cleaning and creating new variables.
# 2. Joins weather data (temperature and precipitation) to the farm-level dataset.
# 3. Joins input and output price data to the dataset.
# 4. Plots the geographic distribution of farm municipalities on a map of Italy.
# 5. Computes descriptive statistics for key variables.
# 6. Estimates a reduced-form regression of yield on weather variables.
# 7. Estimates a structural model using Seemingly Unrelated Regression (SUR).
# 8. Uses the estimated parameters to compute elasticities.  

# ---- preliminary stuff -------------------------------------------------------
rm(list=ls())
gc()

# load packages
wants <- c("tidyverse", "readr", "lfe", "readxl", "RColorBrewer", "systemfit", 
           "DescTools", "rnaturalearth", "rnaturalearthdata","sf", "conleyreg", "fixest")
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
  geom_boxplot(fill = "lightyellow") +
  labs(title = "Distribution of Yield Over Regions", x = "Region", y = "Yield per ha") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

# boxplot of fertiliser over regions for visual inspection
ggplot(d, aes(x = factor(region), y = fertiliser_per_area)) + 
  geom_boxplot(fill = "lightyellow") +
  labs(title = "Distribution of fertiliser per ha Over Regions", x = "Region", y = "Fertiliser per ha") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

# winsorize variables with outliers
d <- d %>%
  mutate(across(c(yield, fertiliser_per_area, pesticide_per_area), ~ Winsorize(., val = quantile(., probs = c(0.01, 0.99), na.rm = TRUE))))

# final number of farms
length(unique(d$farm_code))

# ---- join weather data -------------------------------------------------------
# Choose months based on the growing season

# load monthly temperatures
temps <- readRDS("Data/Raw data/Temperature/Wheat monthly temperatures RICA unbiased.rds")
temps <- temps %>% ungroup()
temps$code <- as.character(temps$code)
temps <- temps %>% rename(mun_code = code,
                          year = data_anno)
temps[, 3:14] <- round(temps[, 3:14], 2) 

# assign oct, nov and dec to the successive growing season (ie add one year to them)

# Reshape the table to long format
temps <- temps %>%
  pivot_longer(cols = starts_with("month"),  # Select all columns that start with 'temp' (i.e., temp1, temp2, ...)
               names_to = "month",          # The new column name for months
               names_prefix = "month",       # Remove the 'temp' prefix from the month numbers
               values_to = "temperature")   # The new column name for temperature values

# add one year to oct to dec
temps <- temps %>%
  mutate(
    year = case_when(
      month %in% 1:9 ~ year,
      month %in% 10:12 ~ year + 1,
      TRUE ~ NA_real_
    )
  )

# now we go back to wide
temps <- temps %>%
  pivot_wider(names_from = month,          # The column that contains the month numbers
              values_from = temperature,   # The column that contains the temperature values
              names_prefix = "month")      # Add the prefix 'temp' back to the month numbers

# compute whole growing season mean temperature
temps <- temps %>%
  mutate(mean_temp = rowMeans(select(., c(month1, month2, month3, month4,
                                          month5, month6, month7, month10,
                                          month11, month12)), na.rm = TRUE)) %>%
  select(mun_code, year, mean_temp)


# load monthly precipitation
prec <- read_csv("Data/Raw data/Precipitation/Wheat monthly precipitation.csv")
prec$mun_code <- as.character(prec$mun_code)

# assign oct, nov and dec to the successive growing season (ie add one year to them)

# Reshape the table to long format
prec <- prec %>%
  pivot_longer(cols = starts_with("prec"),  # Select all columns that start with 'temp' (i.e., temp1, temp2, ...)
               names_to = "month",          # The new column name for months
               names_prefix = "prec",       # Remove the 'temp' prefix from the month numbers
               values_to = "precipitation")   # The new column name for temperature values

# add one year to oct to dec
prec <- prec %>%
  mutate(
    year = case_when(
      month %in% 1:9 ~ year,
      month %in% 10:12 ~ year + 1,
      TRUE ~ NA_real_
    )
  )

# now we go back to wide
prec <- prec %>%
  pivot_wider(names_from = month,          # The column that contains the month numbers
              values_from = precipitation,   # The column that contains the temperature values
              names_prefix = "month")       # Add the prefix 'temp' back to the month numbers

# compute total precipitation during the growing season
prec <- prec %>%
  mutate(sum_prec = rowSums(select(., c(month1, month2, month3, month4,
                                        month5, month6, month7, month10,
                                        month11, month12)), na.rm = TRUE)) %>%
  select(mun_code, year, sum_prec)

# join temperatures and precipitation
d <- left_join(d, temps, by = c("mun_code", "year"))
d <- left_join(d, prec, by = c("mun_code", "year"))

# create squares of weather variables
d$mean_temp_2 = d$mean_temp*d$mean_temp
d$sum_prec_2 = d$sum_prec*d$sum_prec

# ---- join input prices -------------------------------------------------------

# import input price data
input_prices <- read.csv("Data/Raw data/ISTAT/Input prices.csv", sep = ";")
input_prices <- input_prices %>%
  rename(price_fertiliser = difesa,
         price_pesticide = concimi
  ) %>%
  mutate(price_fertiliser = price_fertiliser/100,
         price_pesticide = price_pesticide/100
  )

d <- left_join(d,input_prices, by = "year")

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

d <- left_join(d,output_prices, by = "year")

# compute relative prices
d <- d %>% 
  mutate(rel_price_n = price_fertiliser/price_wheat,
         rel_price_ph = price_pesticide/price_wheat,
         rel_price_n2 = rel_price_n*rel_price_n,
         rel_price_ph2 = rel_price_ph*rel_price_ph,
         rel_price_nph = rel_price_n*rel_price_ph)


# save the cleaned and merged dataset for later use
saveRDS(d, "Data/Processed data/Wheat_cleaned_merged.RDS")

################################################################################
# MAPS AND DESCRIPTIVE STATISTICS ##############################################
################################################################################

# ---- plot farms on the map ---------------------------------------------------

# compute mean area used per farm
d_cor <- d %>%
  group_by(farm_code) %>%
  mutate(avg_area = mean(area, na.rm = TRUE)) %>%
  ungroup()

d_cor <- d_cor %>%
  distinct(farm_code, .keep_all = TRUE) %>%
  select(farm_code, mun_code, avg_area, latitude, longitude) %>%
  filter(!if_any(everything(), is.na))

# Load the shapefile for Italy's regions
italy_regions <- ne_states(country = "Italy", returnclass = "sf")

# Convert the farm data to an sf object
farms_sf <- st_as_sf(d_cor, coords = c("longitude", "latitude"), crs = 4326)

# Plot the map with regional borders and farm points
p <- ggplot() +
  geom_sf(data = italy_regions, fill = "#f0f0f0", color = "lightgray") +
  geom_sf(data = farms_sf, color = "darkgreen", aes(size = avg_area), alpha = 0.5) +
  scale_size_continuous(range = c(0.5, 3)) +
  labs(title = "Wheat", size = "Average crop area (ha)") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 12),     
    legend.title = element_text(size = 14),
    plot.title = element_text(hjust = 0.5, size = 20, color = "black"),
    axis.text.x = element_text(size = 10, color = "darkgrey"),
    axis.text.y = element_text(size = 10, color = "darkgrey"),
    axis.title.y = element_text(size = 14),
    axis.title.x = element_text(size = 14, margin = margin(t = 10)),
    plot.margin = margin(10, 10, 10, 10)
  )

p
#ggsave("Output/Wheat/map_wheat.tiff", p, bg = "white", width = 6, height = 8, dpi = 600, compression = "lzw")


# ---- descriptive statistics --------------------------------------------------

# Calculate descriptive statistics
descriptive_stats <- d %>%
  select(mean_temp, sum_prec, yield, area, price_wheat, price_fertiliser, price_pesticide, fertiliser_per_area_deflated, pesticide_per_area_deflated) %>%
  reframe(
    Mean = sapply(., function(x) round(mean(x, na.rm = TRUE), 3)),
    SD = sapply(., function(x) round(sd(x, na.rm = TRUE), 3)),
    Min = sapply(., function(x) round(min(x, na.rm = TRUE), 3)),
    Max = sapply(., function(x) round(max(x, na.rm = TRUE), 3))
  )

descriptive_stats$Variable <- c("Average temperature", "Total precipitation", "Yield", "Area", "Wheat price (2010=100)", "Fertilizer price (2010=1)", "Pesticide price (2010=1)", "Fertilizer applications per Ha", "Pesticide applications per Ha")

descriptive_stats <- descriptive_stats %>%
  select(Variable, Mean, SD, Min, Max)

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
    mean_temp_within = mean_temp - mean(mean_temp),
    mean_temp_2_within = mean_temp_2 - mean(mean_temp_2),
    sum_prec_within = sum_prec - mean(sum_prec),
    sum_prec_2_within = sum_prec_2 - mean(sum_prec_2),
    x_n_within = fertiliser_per_area_deflated - mean(fertiliser_per_area_deflated),
    x_ph_within = pesticide_per_area_deflated - mean(pesticide_per_area_deflated),
    rel_price_n_within = rel_price_n - mean(rel_price_n),
    rel_price_n_mean_temp_within = rel_price_n*mean_temp - mean(rel_price_n*mean_temp),
    rel_price_n_mean_temp_2_within = rel_price_n*mean_temp_2 - mean(rel_price_n*mean_temp_2),
    rel_price_n_sum_prec_within = rel_price_n*sum_prec - mean(rel_price_n*sum_prec),
    rel_price_n_sum_prec_2_within = rel_price_n*sum_prec_2 - mean(rel_price_n*sum_prec_2),
    rel_price_n2_within = rel_price_n2 - mean(rel_price_n2),
    rel_price_n2_mean_temp_within = rel_price_n2*mean_temp - mean(rel_price_n2*mean_temp),
    rel_price_n2_mean_temp_2_within = rel_price_n2*mean_temp_2 - mean(rel_price_n2*mean_temp_2),
    rel_price_n2_sum_prec_within = rel_price_n2*sum_prec - mean(rel_price_n2*sum_prec),
    rel_price_n2_sum_prec_2_within = rel_price_n2*sum_prec_2 - mean(rel_price_n2*sum_prec_2),
    rel_price_ph_within = rel_price_ph - mean(rel_price_ph),
    rel_price_ph_mean_temp_within = rel_price_ph*mean_temp - mean(rel_price_ph*mean_temp),
    rel_price_ph_mean_temp_2_within = rel_price_ph*mean_temp_2 - mean(rel_price_ph*mean_temp_2),
    rel_price_ph_sum_prec_within = rel_price_ph*sum_prec - mean(rel_price_ph*sum_prec),
    rel_price_ph_sum_prec_2_within = rel_price_ph*sum_prec_2 - mean(rel_price_ph*sum_prec_2),
    rel_price_ph2_within = rel_price_ph2 - mean(rel_price_ph2),
    rel_price_ph2_mean_temp_within = rel_price_ph2*mean_temp - mean(rel_price_ph2*mean_temp),
    rel_price_ph2_mean_temp_2_within = rel_price_ph2*mean_temp_2 - mean(rel_price_ph2*mean_temp_2),
    rel_price_ph2_sum_prec_within = rel_price_ph2*sum_prec - mean(rel_price_ph2*sum_prec),
    rel_price_ph2_sum_prec_2_within = rel_price_ph2*sum_prec_2 - mean(rel_price_ph2*sum_prec_2),
    rel_price_nph_within = rel_price_nph - mean(rel_price_nph),
    rel_price_nph_mean_temp_within = rel_price_nph*mean_temp - mean(rel_price_nph*mean_temp),
    rel_price_nph_mean_temp_2_within = rel_price_nph*mean_temp_2 - mean(rel_price_nph*mean_temp_2),
    rel_price_nph_sum_prec_within = rel_price_nph*sum_prec - mean(rel_price_nph*sum_prec),
    rel_price_nph_sum_prec_2_within = rel_price_nph*sum_prec_2 - mean(rel_price_nph*sum_prec_2)
  ) %>% 
  ungroup()

# avoid results in scientific notation
options(scipen = 999)


# ---- reduced form ------------------------------------------------------------

# with fixest
model_Y_fixest <- feols(yield ~ mean_temp + mean_temp_2 + sum_prec + sum_prec_2 | farm_code, data = d)
summary(model_Y_fixest)
summary(model_Y_fixest, vcov = "hetero")
d$agr_reg_year <- interaction(d$agr_reg_code, d$year, drop = TRUE)
summary(model_Y_fixest, vcov = ~agr_reg_year)

# conley se
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
  a_temp*mean_temp_within + a_temp2*mean_temp_2_within +
  a_prec*sum_prec_within + a_prec2*sum_prec_2_within -
  .5*(d11_0*rel_price_ph2_within + d11_temp*rel_price_ph2_mean_temp_within +
        d11_temp2*rel_price_ph2_mean_temp_2_within + d11_prec*rel_price_ph2_sum_prec_within +
        d11_prec2*rel_price_ph2_sum_prec_2_within) -
  .5*(d22_0*rel_price_n2_within + d22_temp*rel_price_n2_mean_temp_within +
        d22_temp2*rel_price_n2_mean_temp_2_within + d22_prec*rel_price_n2_sum_prec_within +
        d22_prec2*rel_price_n2_sum_prec_2_within) +
  (d12_0*rel_price_nph_within + d12_temp*rel_price_nph_mean_temp_within +
     d12_temp2*rel_price_nph_mean_temp_2_within + d12_prec*rel_price_nph_sum_prec_within +
     d12_prec2*rel_price_nph_sum_prec_2_within)

x_n_within.formula <- x_n_within ~
  b1_temp*mean_temp_within + b1_temp2*mean_temp_2_within +
  b1_prec*sum_prec_within + b1_prec2*sum_prec_2_within -
  (d22_0*rel_price_n_within + d22_temp*rel_price_n_mean_temp_within +
     d22_temp2*rel_price_n_mean_temp_2_within + d22_prec*rel_price_n_sum_prec_within +
     d22_prec2*rel_price_n_sum_prec_2_within) +
  (d12_0*rel_price_ph_within + d12_temp*rel_price_ph_mean_temp_within +
     d12_temp2*rel_price_ph_mean_temp_2_within + d12_prec*rel_price_ph_sum_prec_within +
     d12_prec2*rel_price_ph_sum_prec_2_within)

x_ph_within.formula <- x_ph_within ~
  b2_temp*mean_temp_within + b2_temp2*mean_temp_2_within +
  b2_prec*sum_prec_within + b2_prec2*sum_prec_2_within -
  (d11_0*rel_price_ph_within + d11_temp*rel_price_ph_mean_temp_within +
     d11_temp2*rel_price_ph_mean_temp_2_within + d11_prec*rel_price_ph_sum_prec_within +
     d11_prec2*rel_price_ph_sum_prec_2_within) +
  (d12_0*rel_price_n_within + d12_temp*rel_price_n_mean_temp_within +
     d12_temp2*rel_price_n_mean_temp_2_within + d12_prec*rel_price_n_sum_prec_within +
     d12_prec2*rel_price_n_sum_prec_2_within)

# define starting values for convergence
start.values <- c(
  # α 
  a_temp=100, a_temp2=-1, a_prec=-1, a_prec2=-1,
  # δ_(2,2) 
  d22_0=-1000, d22_temp=1000, d22_temp2=-100, d22_prec=-1, d22_prec2=1,
  # δ_(1,1) 
  d11_0=1000, d11_temp=500, d11_temp2=-10, d11_prec=-10, d11_prec2=1,
  # δ_(1,2) 
  d12_0=-1000, d12_temp=100, d12_temp2=-10, d12_prec=-1, d12_prec2=1,
  # β_1 
  b1_temp=-10, b1_temp2=1, b1_prec=1, b1_prec2=0.01,
  # β_2 
  b2_temp=10, b2_temp2=-1, b2_prec=-1, b2_prec2=0.1
)

if(F){ # safeguard since this takes a while to run, use only first time you run the code or if you want to re-estimate the model
  model_within <- list(yield_within.formula, x_n_within.formula, x_ph_within.formula)
  structural_model <- nlsystemfit("SUR", model_within, start.values, data=d)
  saveRDS(structural_model, "Output/Wheat/Wheat_structural_model.RDS")
}

# use already saved model
structural_model <- readRDS("Output/Wheat/Wheat_structural_model.RDS")

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

#write.csv(summary_table, "Output/Wheat/Wheat_summary_structural.csv", row.names= T,quote=FALSE)


# ---- marginal effects at different percentiles of the temp distribution ------

{
  # find the temperatures for the percentiles of the distribution
  farm_temps <- d %>%
    group_by(farm_code) %>%
    summarise(farm_temp = mean(mean_temp, na.rm = TRUE))
  
  temp_quantiles <- as.numeric(quantile(farm_temps$farm_temp, probs = c(0.25, 0.50, 0.75)))
  print(temp_quantiles)
  
  # define functions and apply to quantiles
  me_yield_st <- function(x) (structural_model$b[1]+2*structural_model$b[2]*x -0.5*mean(d$rel_price_n)*mean(d$rel_price_n)*(structural_model$b[6]+2*structural_model$b[7]*x)- 0.5*mean(d$rel_price_ph)*mean(d$rel_price_ph)*(structural_model$b[11]+2*structural_model$b[12]*x) + mean(d$rel_price_n)*mean(d$rel_price_ph)*(structural_model$b[16]+2*structural_model$b[17]*x))*x/mean(d$yield)
  me_yield_st_app = sapply(temp_quantiles, me_yield_st)
  
  gradient_st <- function(x) c(1,2*x,0,0,
                               0,-0.5*mean(d$rel_price_n)*mean(d$rel_price_n),-mean(d$rel_price_n)*mean(d$rel_price_n)*x,0,0,
                               0,-0.5*mean(d$rel_price_ph)*mean(d$rel_price_ph),-mean(d$rel_price_ph)*mean(d$rel_price_ph)*x,0,0,
                               0,mean(d$rel_price_ph)*mean(d$rel_price_n),mean(d$rel_price_ph)*mean(d$rel_price_n)*2*x,0,0,
                               0,0,0,0,
                               0,0,0,0)
  
  me_yield_st_var <- function(x) t(gradient_st(x))%*%structural_model$covb%*%gradient_st(x)
  me_yield_st_sd <- function(x) (me_yield_st_var(x))^0.5*(x/mean(d$yield))
  me_yield_st_sd_app <- sapply(temp_quantiles, me_yield_st_sd)
  
  # retrieve coefficients and vcov matrix
  beta_rd <- model_Y_fixest %>% 
    coef()
  
  vcov_rd <- vcov(model_Y_fixest)
  
  me_yield_rd <- function(x) (beta_rd[2] + 2*beta_rd[3]*x)*(x/mean(d$yield))
  me_yield_rd_app <- sapply(temp_quantiles, me_yield_rd)
  
  gradient_rd <- function(x) c(0, 1, 2*x, 0, 0)
  
  me_yield_rd_var <- function(x) t(gradient_rd(x)) %*% vcov_rd %*% gradient_rd(x)
  me_yield_rd_sd <- function(x) (me_yield_rd_var(x))^0.5*(x/mean(d$yield))
  me_yield_rd_sd_app <- sapply(temp_quantiles, me_yield_rd_sd)
  
  # create vector combining me and sd of reduced and structural model
  me_plot <- c(rbind(me_yield_rd_app, me_yield_st_app))
  sd_plot <- c(rbind(me_yield_rd_sd_app, me_yield_st_sd_app))
  
  data_me <- data.frame(
    model = factor(rep(c("Reduced form", "Structural model"), 3),
                   levels = c("Reduced form", "Structural model")),
    quartiles = factor(rep(c("First quartile", "Median", "Third quartile"), each = 2), 
                       levels = c("First quartile", "Median", "Third quartile")
    ),
    Max = me_plot + (1.645*sd_plot),   
    Min = me_plot - (1.645*sd_plot), 
    Value = me_plot
  )
  
  plot <- ggplot(data_me, aes(x = quartiles, y = Value, fill = model)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
    geom_errorbar(aes(ymin = Min, ymax = Max), 
                  position = position_dodge(width = 0.8), width = 0.25, color = "black") + 
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  
    scale_fill_manual(values = c("gold2", "red3"), name = "") +  
    scale_y_continuous(limits = c(-0.7, 0.5), breaks = seq(-0.7, 0.5, by = 0.2), expand = c(0, 0)) +
    labs(
      title = "Wheat",
      x = "Growing season mean Temperature",
      y = "Elasticity"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      legend.text = element_text(size = 12),       
      legend.title = element_text(size = 14),
      plot.title = element_text(hjust = 0.5, size = 20, color = "black"),
      axis.text.x = element_text(size = 14, color = "black"),
      axis.text.y = element_text(size = 14, color = "black"),
      axis.title.y = element_text(size = 14),
      axis.title.x = element_text(size = 14, margin = margin(t = 10)),  
      plot.margin = margin(10, 10, 10, 10)
    )
  plot
}

#ggsave(filename = "Output/Wheat/ME_Wheat.tiff", plot, width = 8, height = 8, dpi = 600, bg = "white", compression = "lzw")


# ---- plot demand for fertilizer ----------------------------------------------

{
  # Define the temperature sequence (change the range according to data)
  temp_seq <- seq(8, 16, length.out = 100)
  mean_temp_mean <- mean(d$mean_temp, na.rm = TRUE)
  
  eq_fertiliser_per_area_deflated = function(x){mean(d$fertiliser_per_area_deflated)*mean(d$price_fertiliser) + (structural_model$b[20]*(x-mean(d$mean_temp)) + structural_model$b[21]*(x*x-mean(d$mean_temp_2)) - mean(d$rel_price_n)*(structural_model$b[6]*(x-mean(d$mean_temp)) + structural_model$b[7]*(x*x-mean(d$mean_temp_2))) + mean(d$rel_price_ph)*(structural_model$b[16]*(x-mean(d$mean_temp)) +structural_model$b[17]*(x*x-mean(d$mean_temp_2))))*mean(d$price_fertiliser)}
  eq_fertiliser_per_area_deflated_app = sapply(temp_seq, eq_fertiliser_per_area_deflated)
  
  eq_fertiliser_per_area_deflated_up = function(x){mean(d$price_fertiliser)*1.645*as.numeric((t(c(0,0,0,0,
                                                                                            0,-mean(d$rel_price_n)*((x-mean(d$mean_temp))^2)^0.5,-mean(d$rel_price_n)*((x*x-mean(d$mean_temp_2))^2)^0.5,0,0,
                                                                                            0,0,0,0,0,
                                                                                            0,mean(d$rel_price_ph)*((x-mean(d$mean_temp))^2)^0.5,mean(d$rel_price_ph)*((x*x-mean(d$mean_temp_2))^2)^0.5,0,0,
                                                                                            ((x-mean(d$mean_temp))^2)^0.5,((x*x-mean(d$mean_temp_2))^2)^0.5,0,0,
                                                                                            0,0,0,0))%*%structural_model$covb%*%c(0,0,0,0,
                                                                                                                                  0,-mean(d$rel_price_n)*((x-mean(d$mean_temp))^2)^0.5,-mean(d$rel_price_n)*((x*x-mean(d$mean_temp_2))^2)^0.5,0,0,
                                                                                                                                  0,0,0,0,0,
                                                                                                                                  0,mean(d$rel_price_ph)*((x-mean(d$mean_temp))^2)^0.5,mean(d$rel_price_ph)*((x*x-mean(d$mean_temp_2))^2)^0.5,0,0,
                                                                                                                                  ((x-mean(d$mean_temp))^2)^0.5,((x*x-mean(d$mean_temp_2))^2)^0.5,0,0,
                                                                                                                                  0,0,0,0))^0.5) + eq_fertiliser_per_area_deflated(x)}
  eq_fertiliser_per_area_deflated_up_app=sapply(temp_seq, eq_fertiliser_per_area_deflated_up)
  
  eq_fertiliser_per_area_deflated_down = function(x){-mean(d$price_fertiliser)*1.645*as.numeric((t(c(0,0,0,0,
                                                                                               0,-mean(d$rel_price_n)*((x-mean(d$mean_temp))^2)^0.5,-mean(d$rel_price_n)*((x*x-mean(d$mean_temp_2))^2)^0.5,0,0,
                                                                                               0,0,0,0,0,
                                                                                               0,mean(d$rel_price_ph)*((x-mean(d$mean_temp))^2)^0.5,mean(d$rel_price_ph)*((x*x-mean(d$mean_temp_2))^2)^0.5,0,0,
                                                                                               ((x-mean(d$mean_temp))^2)^0.5,((x*x-mean(d$mean_temp_2))^2)^0.5,0,0,
                                                                                               0,0,0,0))%*%structural_model$covb%*%c(0,0,0,0,
                                                                                                                                     0,-mean(d$rel_price_n)*((x-mean(d$mean_temp))^2)^0.5,-mean(d$rel_price_n)*((x*x-mean(d$mean_temp_2))^2)^0.5,0,0,
                                                                                                                                     0,0,0,0,0,
                                                                                                                                     0,mean(d$rel_price_ph)*((x-mean(d$mean_temp))^2)^0.5,mean(d$rel_price_ph)*((x*x-mean(d$mean_temp_2))^2)^0.5,0,0,
                                                                                                                                     ((x-mean(d$mean_temp))^2)^0.5,((x*x-mean(d$mean_temp_2))^2)^0.5,0,0,
                                                                                                                                     0,0,0,0))^0.5) + eq_fertiliser_per_area_deflated(x)}
  eq_fertiliser_per_area_deflated_down_app=sapply(temp_seq, eq_fertiliser_per_area_deflated_down)
  
  
  # Create a data frame for plotting
  plot_data <- data.frame(temp_seq = temp_seq,
                          predicted_ln_yield = eq_fertiliser_per_area_deflated_app,
                          upper_bound = eq_fertiliser_per_area_deflated_up_app,
                          lower_bound = eq_fertiliser_per_area_deflated_down_app)
  
  # Plot the response function with confidence bands
  fertilizer_demand <- ggplot(plot_data, aes(x = temp_seq, y = predicted_ln_yield)) +
    geom_line(color = "darkgreen", size = 1) +
    geom_ribbon(aes(ymin = lower_bound, ymax = upper_bound),
                alpha = 0.2, fill = "darkgreen") +
    geom_vline(xintercept = mean_temp_mean, color = "black", linetype = "dotted") + 
    labs(x = "Mean Temperature", y = "Fertilizer use", 
         title = "Response of fertilizer demand to Mean Temperature - Structural model") +
    theme_minimal()
  
  fertilizer_demand
  #ggsave("Output/Wheat/Wheat_fertilizer_demand.png", plot = fertilizer_demand, width = 8, height = 6)
}

# ---- plot demand for pesticides ----------------------------------------------

{
  # Define the temperature sequence (change the range according to your data)
  temp_seq <- seq(8, 16, length.out = 100)
  mean_temp_mean <- mean(d$mean_temp, na.rm = TRUE)
  
  eq_blph = function(x){mean(d$pesticide_per_area_deflated)*mean(d$price_pesticide) + (structural_model$b[24]*(x-mean(d$mean_temp)) + structural_model$b[25]*(x*x-mean(d$mean_temp_2)) - mean(d$rel_price_ph)*(structural_model$b[11]*(x-mean(d$mean_temp)) + structural_model$b[12]*(x*x-mean(d$mean_temp_2))) + mean(d$rel_price_n)*(structural_model$b[16]*(x-mean(d$mean_temp)) + structural_model$b[17]*(x*x-mean(d$mean_temp_2))))*mean(d$price_pesticide)}
  eq_blph_app = sapply(temp_seq, eq_blph)
  
  eq_blph_up = function(x){mean(d$price_pesticide)*1.645*as.numeric((t(c(0,0,0,0,
                                                                      0,0,0,0,0,
                                                                      0,-mean(d$rel_price_ph)*((x-mean(d$mean_temp))^2)^0.5,-mean(d$rel_price_ph)*((x*x-mean(d$mean_temp_2))^2)^0.5,0,0,
                                                                      0,mean(d$rel_price_n)*((x-mean(d$mean_temp))^2)^0.5,mean(d$rel_price_n)*((x*x-mean(d$mean_temp_2))^2)^0.5,0,0,
                                                                      0,0,0,0,
                                                                      ((x-mean(d$mean_temp))^2)^0.5,((x*x-mean(d$mean_temp_2))^2)^0.5,0,0))%*%structural_model$covb%*%c(0,0,0,0,
                                                                                                                                                                        0,0,0,0,0,
                                                                                                                                                                        0,-mean(d$rel_price_ph)*((x-mean(d$mean_temp))^2)^0.5,-mean(d$rel_price_ph)*((x*x-mean(d$mean_temp_2))^2)^0.5,0,0,
                                                                                                                                                                        0,mean(d$rel_price_n)*((x-mean(d$mean_temp))^2)^0.5,mean(d$rel_price_n)*((x*x-mean(d$mean_temp_2))^2)^0.5,0,0,
                                                                                                                                                                        0,0,0,0,
                                                                                                                                                                        ((x-mean(d$mean_temp))^2)^0.5,((x*x-mean(d$mean_temp_2))^2)^0.5,0,0))^0.5) + eq_blph(x)}
  eq_blph_up_app=sapply(temp_seq, eq_blph_up)
  
  eq_blph_down = function(x){-mean(d$price_pesticide)*1.645*as.numeric((t(c(0,0,0,0,
                                                                         0,0,0,0,0,
                                                                         0,-mean(d$rel_price_ph)*((x-mean(d$mean_temp))^2)^0.5,-mean(d$rel_price_ph)*((x*x-mean(d$mean_temp_2))^2)^0.5,0,0,
                                                                         0,mean(d$rel_price_n)*((x-mean(d$mean_temp))^2)^0.5,mean(d$rel_price_n)*((x*x-mean(d$mean_temp_2))^2)^0.5,0,0,
                                                                         0,0,0,0,
                                                                         ((x-mean(d$mean_temp))^2)^0.5,((x*x-mean(d$mean_temp_2))^2)^0.5,0,0))%*%structural_model$covb%*%c(0,0,0,0,
                                                                                                                                                                           0,0,0,0,0,
                                                                                                                                                                           0,-mean(d$rel_price_ph)*((x-mean(d$mean_temp))^2)^0.5,-mean(d$rel_price_ph)*((x*x-mean(d$mean_temp_2))^2)^0.5,0,0,
                                                                                                                                                                           0,mean(d$rel_price_n)*((x-mean(d$mean_temp))^2)^0.5,mean(d$rel_price_n)*((x*x-mean(d$mean_temp_2))^2)^0.5,0,0,
                                                                                                                                                                           0,0,0,0,
                                                                                                                                                                           ((x-mean(d$mean_temp))^2)^0.5,((x*x-mean(d$mean_temp_2))^2)^0.5,0,0))^0.5) + eq_blph(x)}
  eq_blph_down_app=sapply(temp_seq, eq_blph_down)
  
  # Create a data frame for plotting
  plot_data <- data.frame(temp_seq = temp_seq,
                          predicted_ln_yield = eq_blph_app,
                          upper_bound = eq_blph_up_app,
                          lower_bound = eq_blph_down_app)
  
  # Plot the response function with confidence bands
  pesticide_demand <- ggplot(plot_data, aes(x = temp_seq, y = predicted_ln_yield)) +
    geom_line(color = "darkred", size = 1) +
    geom_ribbon(aes(ymin = lower_bound, ymax = upper_bound),
                alpha = 0.2, fill = "darkred") +
    geom_vline(xintercept = mean_temp_mean, color = "black", linetype = "dotted") + 
    labs(x = "Mean Temperature", y = "Pesticides use", 
         title = "Response of pesticides demand to Mean Temperature - Structural model") +
    theme_minimal()
  
  pesticide_demand
  #ggsave("Output/Wheat/Wheat_pesticide_demand.png", plot = pesticide_demand, width = 8, height = 6)
}

# ---- check convexity of profit set -------------------------------------------

{
  gamma_fert_bl=structural_model$b[5] + structural_model$b[6]*(mean(d$mean_temp)) + structural_model$b[7]*(mean(d$mean_temp_2)) + structural_model$b[8]*(mean(d$sum_prec)) + structural_model$b[9]*(mean(d$sum_prec_2))
  gamma_pest_bl=structural_model$b[10] + structural_model$b[11]*(mean(d$mean_temp)) + structural_model$b[12]*(mean(d$mean_temp_2)) + structural_model$b[13]*(mean(d$sum_prec)) + structural_model$b[14]*(mean(d$sum_prec_2)) 
  gamma_fert_pest_bl=structural_model$b[15] + structural_model$b[16]*(mean(d$mean_temp)) + structural_model$b[17]*(mean(d$mean_temp_2)) + structural_model$b[18]*(mean(d$sum_prec)) + structural_model$b[19]*(mean(d$sum_prec_2))
  delta_bl=(structural_model$b[5] + structural_model$b[6]*(mean(d$mean_temp)) + structural_model$b[7]*(mean(d$mean_temp_2)) + structural_model$b[8]*(mean(d$sum_prec)) + structural_model$b[9]*(mean(d$sum_prec_2))) * (structural_model$b[10] + structural_model$b[11]*(mean(d$mean_temp)) + structural_model$b[12]*(mean(d$mean_temp_2)) + structural_model$b[13]*(mean(d$sum_prec)) + structural_model$b[14]*(mean(d$sum_prec_2))) - (structural_model$b[15] + structural_model$b[16]*(mean(d$mean_temp)) + structural_model$b[17]*(mean(d$mean_temp_2)) + structural_model$b[18]*(mean(d$sum_prec)) + structural_model$b[19]*(mean(d$sum_prec_2)))^2
  beta_fert_bl_real = mean(d$fertiliser_per_area_deflated) + gamma_fert_bl*mean(d$rel_price_n) - gamma_fert_pest_bl*mean(d$rel_price_ph)
  beta_pest_bl_real = mean(d$pesticide_per_area_deflated) + gamma_pest_bl*mean(d$rel_price_ph) - gamma_fert_pest_bl*mean(d$rel_price_n)
  aggregate_productivity_fert_ble = (gamma_fert_bl*(beta_fert_bl_real - mean(d$fertiliser_per_area_deflated) )-gamma_fert_pest_bl*(beta_pest_bl_real - mean(d$pesticide_per_area_deflated)))/delta_bl
  aggregate_productivity_pest_ble = (gamma_pest_bl*(beta_pest_bl_real - mean(d$pesticide_per_area_deflated) )-gamma_fert_pest_bl*(beta_fert_bl_real - mean(d$fertiliser_per_area_deflated)))/delta_bl
  
  # Create data frame for elasticities
  productivity_parameters <- data.frame(
    Category = c("Own productivity fertilizer", "Own productivity pesticide", "Complementarity/substitution term", "Concavity", "Aggr. productivity fertilizer", "Aggr. productivity pesticide"),
    Parameters = c(
      round(gamma_fert_bl, 3), 
      round(gamma_pest_bl, 3),
      round(gamma_fert_pest_bl, 3),
      round(delta_bl, 3),
      round(aggregate_productivity_fert_ble, 3),
      round(aggregate_productivity_pest_ble, 3)
    )
  )
  
  print(productivity_parameters)
  #write.csv(productivity_parameters, "Output/Wheat/Wheat_productivity_parameters.csv", row.names= F,quote=FALSE)
}

# ---- input use elasticities at different temperature quantiles ---------------

# define the temperature quantiles we are interested in
temp_quantiles <- quantile(d$mean_temp, probs = c(0.25, 0.50, 0.75))

# fertiliser
el_fertiliser_temp_fun <- function(x) (structural_model$b[20]+2*structural_model$b[21]*x - mean(d$rel_price_n)*(structural_model$b[6]+2*structural_model$b[7]*x) + mean(d$rel_price_ph)*(structural_model$b[16]+2*structural_model$b[17]*x))*x/mean(d$fertiliser_per_area_deflated)
el_fertiliser_temp_app <- sapply(temp_quantiles, el_fertiliser_temp_fun)

grad_fertiliser_temp_fun <- function(x) c(0,0,0,0,
                                       0,-mean(d$rel_price_n),-mean(d$rel_price_n)*2*x,0,0,
                                       0,0,0,0,0,
                                       0,mean(d$rel_price_ph),mean(d$rel_price_ph)*2*x,0,0,
                                       1,2*x,0,0,
                                       0,0,0,0)


el_fertiliser_temp_v_fun <- function(x) t(grad_fertiliser_temp_fun(x))%*%structural_model$covb%*%grad_fertiliser_temp_fun(x)

el_fertiliser_temp_sd_fun <- function(x) (x/mean(d$fertiliser_per_area_deflated))*(el_fertiliser_temp_v_fun(x))^0.5
el_fertiliser_temp_sd_app <- sapply(temp_quantiles, el_fertiliser_temp_sd_fun)

print(cbind(el_fertiliser_temp_app, el_fertiliser_temp_sd_app))

# print
print(cbind(paste0(round(el_fertiliser_temp_app,3), " (", round(el_fertiliser_temp_sd_app,3), ")")))

#compute t stat for significance
t_stat_fertiliser_temp=el_fertiliser_temp_app/el_fertiliser_temp_sd_app
print(t_stat_fertiliser_temp)

# pesticide
el_pesticide_temp_fun <- function(x) (structural_model$b[24]+2*structural_model$b[25]*x - mean(d$rel_price_ph)*(structural_model$b[11]+2*structural_model$b[12]*x) + mean(d$rel_price_n)*(structural_model$b[16]+2*structural_model$b[17]*x))*x/mean(d$pesticide_per_area_deflated)
el_pesticide_temp_app <- sapply(temp_quantiles, el_pesticide_temp_fun)

grad_pesticide_temp_fun <- function(x) c(0,0,0,0,
                                      0,0,0,0,0,
                                      0,-mean(d$rel_price_ph),-mean(d$rel_price_ph)*2*x,0,0,
                                      0,mean(d$rel_price_n),mean(d$rel_price_n)*2*x,0,0,
                                      0,0,0,0,
                                      1,2*x,0,0)

el_pesticide_temp_v_fun <- function(x) t(grad_pesticide_temp_fun(x))%*%structural_model$covb%*%grad_pesticide_temp_fun(x)
el_pesticide_temp_sd_fun <- function(x) (x/mean(d$pesticide_per_area_deflated))*(el_pesticide_temp_v_fun(x))^0.5
el_pesticide_temp_sd_app <- sapply(temp_quantiles, el_pesticide_temp_sd_fun)

print(cbind(el_pesticide_temp_app, el_pesticide_temp_sd_app))

# print
print(cbind(paste0(round(el_pesticide_temp_app,3), " (", round(el_pesticide_temp_sd_app,3), ")")))


# ---- agronomic and adaptation effects ----------------------------------------

{
  # define temp quantiles
  farm_temps <- d %>%
    group_by(farm_code) %>%
    summarise(farm_temp = mean(mean_temp, na.rm = TRUE))
  
  temp_quantiles <- as.numeric(quantile(farm_temps$farm_temp, probs = c(0.25, 0.50, 0.75)))
  print(temp_quantiles)
  
  
  gamma_n <- function(x) structural_model$b[5] + structural_model$b[6]*x + structural_model$b[7]*x^2 + structural_model$b[8]*(mean(d$sum_prec)) + structural_model$b[9]*(mean(d$sum_prec_2))
  gamma_p <- function(x) structural_model$b[10] + structural_model$b[11]*x + structural_model$b[12]*x^2 + structural_model$b[13]*(mean(d$sum_prec)) + structural_model$b[14]*(mean(d$sum_prec_2)) 
  gamma_np <- function(x) structural_model$b[15] + structural_model$b[16]*x + structural_model$b[17]*x^2 + structural_model$b[18]*(mean(d$sum_prec)) + structural_model$b[19]*(mean(d$sum_prec_2))
  delta <- function(x) (structural_model$b[5] + structural_model$b[6]*x + structural_model$b[7]*x^2 + structural_model$b[8]*(mean(d$sum_prec)) + structural_model$b[9]*(mean(d$sum_prec_2))) * (structural_model$b[10] + structural_model$b[11]*x + structural_model$b[12]*x^2 + structural_model$b[13]*(mean(d$sum_prec)) + structural_model$b[14]*(mean(d$sum_prec_2))) - (structural_model$b[15] + structural_model$b[16]*x + structural_model$b[17]*x^2 + structural_model$b[18]*(mean(d$sum_prec)) + structural_model$b[19]*(mean(d$sum_prec_2)))^2
  beta_n_real <- function(x) mean(d$fertiliser_per_area_deflated) + gamma_n(x)*mean(d$rel_price_n) - gamma_np(x)*mean(d$rel_price_ph)
  beta_p_real <- function(x) mean(d$pesticide_per_area_deflated) + gamma_p(x)*mean(d$rel_price_ph) - gamma_np(x)*mean(d$rel_price_n)
  
  aggr_prod_n <- function(x) {
    numerator <- (gamma_n(x)*(beta_n_real(x) - mean(d$fertiliser_per_area_deflated)) - gamma_np(x)*(beta_p_real(x) - mean(d$pesticide_per_area_deflated)))
    numerator / delta(x)
  }
  aggr_prod_p <- function(x) {
    numerator <- (gamma_p(x)*(beta_p_real(x) - mean(d$pesticide_per_area_deflated)) - gamma_np(x)*(beta_n_real(x) - mean(d$fertiliser_per_area_deflated)))
    numerator / delta(x)
  }
  
  # Temperature elasticities
  el_fertiliser_per_area_deflated_fun <- function(x) (structural_model$b[20]+2*structural_model$b[21]*x - mean(d$rel_price_n)*(structural_model$b[6]+2*structural_model$b[7]*x) + mean(d$rel_price_ph)*(structural_model$b[16]+2*structural_model$b[17]*x))*x/mean(d$fertiliser_per_area_deflated)
  el_fertiliser_per_area_deflated_app <- sapply(temp_quantiles, el_fertiliser_per_area_deflated_fun)
  
  fertiliser_per_area_deflated_grad <- function(x) c(0,0,0,0,
                                                  0,-mean(d$rel_price_n),-mean(d$rel_price_n)*2*x,0,0,
                                                  0,0,0,0,0,
                                                  0,mean(d$rel_price_ph),mean(d$rel_price_ph)*2*x,0,0,
                                                  1,2*x,0,0,
                                                  0,0,0,0)
  
  el_fertiliser_per_area_deflated_var <- function(x) t(fertiliser_per_area_deflated_grad(x))%*%structural_model$covb%*%fertiliser_per_area_deflated_grad(x)
  el_fertiliser_per_area_deflated_sd <- function(x) (x/mean(d$fertiliser_per_area_deflated))*(el_fertiliser_per_area_deflated_var(x))^0.5
  el_fertiliser_per_area_deflated_sd_app <- sapply(temp_quantiles, el_fertiliser_per_area_deflated_sd)
  
  el_pesticide_per_area_deflated_fun <- function(x) (structural_model$b[24]+2*structural_model$b[25]*x - mean(d$rel_price_ph)*(structural_model$b[11]+2*structural_model$b[12]*x) + mean(d$rel_price_n)*(structural_model$b[16]+2*structural_model$b[17]*x))*x/mean(d$pesticide_per_area_deflated)
  el_pesticide_per_area_deflated_app <- sapply(temp_quantiles, el_pesticide_per_area_deflated_fun)
  
  pesticide_per_area_deflated_grad <- function(x) c(0,0,0,0,
                                                 0,0,0,0,0,
                                                 0,-mean(d$rel_price_ph),-mean(d$rel_price_ph)*2*x,0,0,
                                                 0,mean(d$rel_price_n),mean(d$rel_price_n)*2*x,0,0,
                                                 0,0,0,0,
                                                 1,2*x,0,0)
  
  el_pesticide_per_area_deflated_var <- function(x) t(pesticide_per_area_deflated_grad(x))%*%structural_model$covb%*%pesticide_per_area_deflated_grad(x)
  el_pesticide_per_area_deflated_sd <- function(x) (x/mean(d$pesticide_per_area_deflated))*(el_pesticide_per_area_deflated_var(x))^0.5
  el_pesticide_per_area_deflated_sd_app <- sapply(temp_quantiles, el_pesticide_per_area_deflated_sd)
  
  el_yield_fun <- function(x) (structural_model$b[1]+2*structural_model$b[2]*x -0.5*mean(d$rel_price_n)*mean(d$rel_price_n)*(structural_model$b[6]+2*structural_model$b[7]*x)- 0.5*mean(d$rel_price_ph)*mean(d$rel_price_ph)*(structural_model$b[11]+2*structural_model$b[12]*x) + mean(d$rel_price_ph)*mean(d$rel_price_n)*(structural_model$b[16]+2*structural_model$b[17]*x))*x/mean(d$yield)
  el_yield_app <- sapply(temp_quantiles, el_yield_fun)
  
  yield_grad <- function(x) c(1,2*x,0,0,
                              0,-0.5*mean(d$rel_price_n)*mean(d$rel_price_n),-mean(d$rel_price_n)*mean(d$rel_price_n)*x,0,0,
                              0,-0.5*mean(d$rel_price_ph)*mean(d$rel_price_ph),-mean(d$rel_price_ph)*mean(d$rel_price_ph)*x,0,0,
                              0,mean(d$rel_price_ph)*mean(d$rel_price_n),mean(d$rel_price_ph)*mean(d$rel_price_n)*2*x,0,0,
                              0,0,0,0,
                              0,0,0,0)
  
  el_yield_var <- function(x) t(yield_grad(x))%*%structural_model$covb%*%yield_grad(x)
  el_yield_sd <- function(x) (x/mean(d$yield))*(el_yield_var(x))^0.5
  el_yield_sd_app <- sapply(temp_quantiles, el_yield_sd)
  
  el_yield_ad_fun <- function(x) (((el_fertiliser_per_area_deflated_fun(x)))*mean(d$fertiliser_per_area_deflated)*(gamma_n(x)*(beta_n_real(x)-mean(d$fertiliser_per_area_deflated))-gamma_np(x)*(beta_p_real(x)-mean(d$pesticide_per_area_deflated)))/delta(x) + ((el_pesticide_per_area_deflated_fun(x)))*mean(d$pesticide_per_area_deflated)*(gamma_p(x)*(beta_p_real(x)-mean(d$pesticide_per_area_deflated))-gamma_np(x)*(beta_n_real(x)-mean(d$fertiliser_per_area_deflated)))/delta(x))/mean(d$yield)
  el_yield_ad_app <- sapply(temp_quantiles, el_yield_ad_fun)
  
  el_yield_ad_sd <- function(x) (1/mean(d$yield))*((el_fertiliser_per_area_deflated_sd(x)^2)*mean(d$fertiliser_per_area_deflated)*(gamma_n(x)*(beta_n_real(x)-mean(d$fertiliser_per_area_deflated))-gamma_np(x)*(beta_p_real(x)-mean(d$pesticide_per_area_deflated)))/delta(x) + (el_pesticide_per_area_deflated_sd(x)^2)*mean(d$pesticide_per_area_deflated)*(gamma_p(x)*(beta_p_real(x)-mean(d$pesticide_per_area_deflated))-gamma_np(x)*(beta_n_real(x)-mean(d$fertiliser_per_area_deflated)))/delta(x))^0.5
  el_yield_ad_sd_app <- sapply(temp_quantiles, el_yield_ad_sd)
  
  yield_ad_grad <- function(x) c(0,0,0,0,
                                 0,-mean(d$rel_price_n),-mean(d$rel_price_n)*2*x,0,0,
                                 0,-mean(d$rel_price_ph),-mean(d$rel_price_ph)*2*x,0,0,
                                 0,mean(d$rel_price_n)+mean(d$rel_price_ph),(mean(d$rel_price_n)+mean(d$rel_price_ph))*2*x,0,0,
                                 1,2*x,0,0,
                                 1,2*x,0,0)
  el_yield_ag_fun <- function(x) el_yield_fun(x) - el_yield_ad_fun(x)
  el_yield_ag_app <- sapply(temp_quantiles, el_yield_ag_fun)
  
  el_yield_ag_sd <- function(x) (x/mean(d$yield))*((el_yield_sd(x)/(x/mean(d$yield)))^2 + (el_yield_ad_sd(x)/mean(d$yield))^2)^0.5
  el_yield_ag_sd_app <- sapply(temp_quantiles, el_yield_ag_sd)
  
  # Create data frame for elasticities
  agr_elasticities <- data.frame(
    Category = c("Total", "Agronomic effects", "Adaptation effects"),
    Elasticity_1q = c(
      paste0(round(el_yield_app[1], 3), " (", round(el_yield_sd_app[1], 3), ")"),
      paste0(round(el_yield_ag_app[1], 3), " (", round(el_yield_ag_sd_app[1], 3), ")"),
      paste0(round(el_yield_ad_app[1], 3), " (", round(el_yield_ad_sd_app[1], 3), ")")),
    Elasticity_median = c(
      paste0(round(el_yield_app[2], 3), " (", round(el_yield_sd_app[2], 3), ")"),
      paste0(round(el_yield_ag_app[2], 3), " (", round(el_yield_ag_sd_app[2], 3), ")"),
      paste0(round(el_yield_ad_app[2], 3), " (", round(el_yield_ad_sd_app[2], 3), ")")),
    Elasticity_3q = c(
      paste0(round(el_yield_app[3], 3), " (", round(el_yield_sd_app[3], 3), ")"),
      paste0(round(el_yield_ag_app[3], 3), " (", round(el_yield_ag_sd_app[3], 3), ")"),
      paste0(round(el_yield_ad_app[3], 3), " (", round(el_yield_ad_sd_app[3], 3), ")"))
    
  )

  print(agr_elasticities)
  
  }

# ---- end of script -----------------------------------------------------------
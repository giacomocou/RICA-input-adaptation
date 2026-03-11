# using the coefficients computed in 1 and the projections computed in 4,
# estimate the damage in 2050

# ---- preliminary stuff -------------------------------------------------------
rm(list=ls())
gc()

# load packages
wants <- c("tidyverse", "readr")
needs <- wants[!(wants %in% installed.packages()[,"Package"])]
if(length(needs)) install.packages(needs)
lapply(wants, function(i) require(i, character.only=TRUE))
rm(needs,wants)

# ---- load data ---------------------------------------------------------------

# main df
d <- readRDS("Data/Processed data/Wheat_cleaned_merged.RDS")

# structural model
structural_model <- readRDS("Output/Wheat/Wheat_structural_model.RDS")

# read projections ssp370
projections <- readRDS("Data/Processed data/Wheat_projections_unbiased_ssp370.RDS")

# ------------------------------------------------------------------------------

projections <- projections %>%
  rename(province_code = province)

# average yields at province level
d_proj <- d %>%
  group_by(province_code) %>%
  summarize(province = first(province),
            yield = mean(yield, na.rm = TRUE)
  ) %>%
  ungroup()

d_proj <- d_proj %>%
  left_join(
    projections %>% select(province_code, mean_temp_gs, contains("unb")),
    by = "province_code"
  )

# ---- PRE-CALCULATE CONSTANTS AND DEFINE FUNCTIONS ----------------------------

# Intermediate constants
delta_2050 <- (structural_model$b[5]*structural_model$b[10])-(structural_model$b[15])^2

# Helper functions for calculations
fert_2050 <- function(x, temp_today) (structural_model$b[20]*(x-temp_today) + structural_model$b[21]*(x*x-temp_today*temp_today)) 
pest_2050 <- function(x, temp_today) (structural_model$b[24]*(x-temp_today) + structural_model$b[25]*(x*x-temp_today*temp_today)) 

# Yield Change Function
y_2050_bench <- function(x, temp_today, yield_base) {
  yield_base + (structural_model$b[1]*(x-temp_today) + structural_model$b[2]*(x*x-temp_today*temp_today) - 
                  0.5*mean(d$rel_price_n)*mean(d$rel_price_n)*(structural_model$b[6]*(x-temp_today) + structural_model$b[7]*(x*x-temp_today*temp_today)) - 
                  0.5*mean(d$rel_price_ph)*mean(d$rel_price_ph)*(structural_model$b[11]*(x-temp_today) + structural_model$b[12]*(x*x-temp_today*temp_today)) + 
                  mean(d$rel_price_ph)*mean(d$rel_price_n)*(structural_model$b[16]*(x-temp_today) + structural_model$b[17]*(x*x-temp_today*temp_today)))
}

y_2050_main <- function(x, temp_today, yield_base) y_2050_bench(x, temp_today, yield_base) - yield_base

# Agronomic Effects Function
y_2050_ind <- function(x, temp_today) {
  structural_model$b[1]*(x-temp_today) + structural_model$b[2]*(x*x-temp_today*temp_today) - 
    0.5*(((structural_model$b[6]*(x-temp_today) + structural_model$b[7]*(x*x-temp_today*temp_today)))/delta_2050)*((structural_model$b[20]*(x-temp_today) + structural_model$b[21]*(x*x-temp_today*temp_today)))^2 - 
    0.5*((structural_model$b[11]*(x-temp_today) + structural_model$b[12]*(x*x-temp_today*temp_today))/delta_2050)*((structural_model$b[24]*(x-temp_today) + structural_model$b[25]*(x*x-temp_today*temp_today)))^2 + 
    ((structural_model$b[16]*(x-temp_today) + structural_model$b[17]*(x*x-temp_today*temp_today))/delta_2050)*((structural_model$b[20]*(x-temp_today) + structural_model$b[21]*(x*x-temp_today*temp_today) ))*((structural_model$b[24]*(x-temp_today) + structural_model$b[25]*(x*x-temp_today*temp_today)))
}


# ---- SD Calculation Functions ------------------------------------------------

y_2050_sd <- function(x, temp_today) {
  vec <- c(((x-temp_today)^2)^0.5, ((x*x-temp_today*temp_today)^2)^0.5, 0, 0,
           0, -0.5*mean(d$rel_price_n)^2*((x-temp_today)^2)^0.5, -0.5*mean(d$rel_price_n)^2*((x*x-temp_today*temp_today)^2)^0.5, 0, 0,
           0, -0.5*mean(d$rel_price_ph)^2*((x-temp_today)^2)^0.5, -0.5*mean(d$rel_price_ph)^2*((x*x-temp_today*temp_today)^2)^0.5, 0, 0,
           0, mean(d$rel_price_ph)*mean(d$rel_price_n)*((x-temp_today)^2)^0.5, mean(d$rel_price_ph)*mean(d$rel_price_n)*((x*x-temp_today*temp_today)^2)^0.5, 0, 0,
           0, 0, 0, 0, 0, 0, 0, 0)
  as.numeric((t(vec) %*% structural_model$covb %*% vec)^0.5)
}

y_2050_ind_sd <- function(x, temp_today) {
  fert_val <- fert_2050(x, temp_today)
  pest_val <- pest_2050(x, temp_today)
  
  vec <- c(((x-temp_today)^2)^0.5, ((x*x-temp_today*temp_today)^2)^0.5, 0, 0,
           0, -0.5*fert_val^2*((x-temp_today)^2)^0.5/delta_2050, -0.5*fert_val^2*((x*x-temp_today*temp_today)^2)^0.5/delta_2050, 0, 0,
           0, -0.5*pest_val^2*((x-temp_today)^2)^0.5/delta_2050, -0.5*pest_val^2*((x*x-temp_today*temp_today)^2)^0.5/delta_2050, 0, 0,
           0, fert_val*pest_val*((x-temp_today)^2)^0.5/delta_2050, fert_val*pest_val*((x*x-temp_today*temp_today)^2)^0.5/delta_2050, 0, 0,
           0, 0, 0, 0, 0, 0, 0, 0)
  as.numeric((t(vec) %*% structural_model$covb %*% vec)^0.5)
}

cov_2050 = function(x, temp_today) as.numeric((t(c(((x-temp_today)^2)^0.5,((x*x-temp_today*temp_today)^2)^0.5,0,0,
                                                   0,-0.5*fert_2050(x, temp_today)^2*((x-temp_today)^2)^0.5/delta_2050,-0.5*fert_2050(x, temp_today)^2*((x*x-temp_today*temp_today)^2)^0.5/delta_2050,0,0,
                                                   0,-0.5*pest_2050(x, temp_today)^2*((x-temp_today)^2)^0.5/delta_2050,-0.5*pest_2050(x, temp_today)^2*((x*x-temp_today*temp_today)^2)^0.5/delta_2050,0,0,
                                                   0,fert_2050(x, temp_today)*pest_2050(x, temp_today)*((x-temp_today)^2)^0.5/delta_2050,fert_2050(x, temp_today)*pest_2050(x, temp_today)*((x*x-temp_today*temp_today)^2)^0.5/delta_2050,0,0,
                                                   0,0,0,0,
                                                   0,0,0,0))%*%structural_model$covb%*%c(((x-temp_today)^2)^0.5,((x*x-temp_today*temp_today)^2)^0.5,0,0,
                                                                                         0,-0.5*mean(d$rel_price_n)*mean(d$rel_price_n)*((x-temp_today)^2)^0.5,-0.5*mean(d$rel_price_n)*mean(d$rel_price_n)*((x*x-temp_today*temp_today)^2)^0.5,0,0,
                                                                                         0,-0.5*mean(d$rel_price_ph)*mean(d$rel_price_ph)*((x-temp_today)^2)^0.5,-0.5*mean(d$rel_price_ph)*mean(d$rel_price_ph)*((x*x-temp_today*temp_today)^2)^0.5,0,0,
                                                                                         0,mean(d$rel_price_ph)*mean(d$rel_price_n)*((x-temp_today)^2)^0.5,mean(d$rel_price_ph)*mean(d$rel_price_n)*((x*x-temp_today*temp_today)^2)^0.5,0,0,
                                                                                         0,0,0,0,
                                                                                         0,0,0,0))^0.5)


# The combined SD function
y_2050_ad_sd <- function(x, temp_today) {
  (y_2050_sd(x, temp_today)^2 + y_2050_ind_sd(x, temp_today)^2 - 2*cov_2050(x, temp_today))^0.5
}


# ---- LOOP THROUGH SCENARIOS AND SAVE RESULTS ---------------------------------

# Identify all scenario columns (everything with "unb")
scenario_columns <- grep("unb", names(d_proj), value = TRUE)

# Loop through each scenario
for (scen_name in scenario_columns) {
  
  message(paste("Processing scenario:", scen_name))
  
  # Select the temperature vector
  temp_2050_vec <- d_proj[[scen_name]]
  
  # Main Yield Changes
  y_2050_app <- mapply(y_2050_main, temp_2050_vec, d_proj$mean_temp_gs, d_proj$yield)
  
  # Agronomic Effects
  y_2050_ind_app <- mapply(y_2050_ind, temp_2050_vec, d_proj$mean_temp_gs)
  
  # Adaptation Effects
  y_2050_ad_app <- y_2050_app - y_2050_ind_app
  
  # Standard Deviations
  y_2050_sd_app     <- mapply(y_2050_sd, temp_2050_vec, d_proj$mean_temp_gs)
  y_2050_ind_sd_app <- mapply(y_2050_ind_sd, temp_2050_vec, d_proj$mean_temp_gs)
  y_2050_ad_sd_app  <- mapply(y_2050_ad_sd, temp_2050_vec, d_proj$mean_temp_gs)
  
  # create dataframe
  projections_and_sd <- d_proj %>%
    mutate(
      y_2050_total         = y_2050_app,
      y_2050_total_sd      = y_2050_sd_app,
      y_2050_agro          = y_2050_ind_app,
      y_2050_agro_sd       = y_2050_ind_sd_app,
      y_2050_adaptation    = y_2050_ad_app,
      y_2050_adaptation_sd = y_2050_ad_sd_app,
      scenario_used        = scen_name
    )
  
  # save dataframe
  file_name <- paste0("Output/Wheat/Projections/Wheat_projections_ssp370_2050_", scen_name, ".RDS")
  
  saveRDS(projections_and_sd, file_name)
}
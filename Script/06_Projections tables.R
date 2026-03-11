### Script for CORN & WHEAT: Tables of provincial projections ##################

# ---- preliminary stuff -------------------------------------------------------
rm(list=ls())
gc()

# Load packages
wants <- c("tidyverse", "readr", "writexl")
needs <- wants[!(wants %in% installed.packages()[,"Package"])]
if(length(needs)) install.packages(needs)
lapply(wants, function(i) require(i, character.only=TRUE))
rm(needs,wants)

# ---- CORN --------------------------------------------------------------------

Corn_proj <- readRDS("Output/Corn/Projections/Corn_projections_ssp370_2050_ensemble_unb.RDS")

Corn_proj <- Corn_proj %>%
  mutate(across(where(is.numeric), ~ format(round(.x, 2), nsmall = 2))) %>%
  mutate(across(everything(), as.character)) %>%
  select(-scenario_used, -province_code, -y_2050_adaptation, -y_2050_adaptation_sd) %>%
  mutate(across(c(y_2050_total_sd, y_2050_agro_sd), ~ paste0("(", .x, ")"))) %>%
  mutate(y_2050_total = paste(y_2050_total, y_2050_total_sd),
         y_2050_agro = paste(y_2050_agro, y_2050_agro_sd)) %>%
  select(-c(y_2050_total_sd, y_2050_agro_sd))

write_xlsx(Corn_proj, "Output/Corn/Projections/Corn_projections_ssp370_2050_table.xlsx")

# ---- WHEAT -------------------------------------------------------------------

wheat_proj <- readRDS("Output/Wheat/Projections/Wheat_projections_ssp370_2050_ensemble_unb.RDS")

wheat_proj <- wheat_proj %>%
  mutate(across(where(is.numeric), ~ format(round(.x, 2), nsmall = 2))) %>%
  mutate(across(everything(), as.character)) %>%
  select(-scenario_used, -province_code, -y_2050_adaptation, -y_2050_adaptation_sd) %>%
  mutate(across(c(y_2050_total_sd, y_2050_agro_sd), ~ paste0("(", .x, ")"))) %>%
  mutate(y_2050_total = paste(y_2050_total, y_2050_total_sd),
         y_2050_agro = paste(y_2050_agro, y_2050_agro_sd)) %>%
  select(-c(y_2050_total_sd, y_2050_agro_sd))

write_xlsx(wheat_proj, "Output/Wheat/Projections/Wheat_projections_ssp370_2050_table.xlsx")

# ---- end of script -----------------------------------------------------------
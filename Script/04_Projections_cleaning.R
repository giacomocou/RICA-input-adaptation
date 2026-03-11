### Script for CORN & WHEAT: Bias removal for projections ######################

# ---- preliminary stuff -------------------------------------------------------
rm(list=ls())
gc()

# Load packages
wants <- c("tidyverse", "readr", "readxl")
needs <- wants[!(wants %in% installed.packages()[,"Package"])]
if(length(needs)) install.packages(needs)
lapply(wants, function(i) require(i, character.only=TRUE))
rm(needs,wants)

# ---- PROJECTIONS DATA --------------------------------------------------------
corn_hist <- read_csv("Data/Raw data/Projections/corn_hist_1990_2010.csv")
corn_proj <- read_csv("Data/Raw data/Projections/corn_ssp370_2040_2060.csv")
wheat_hist <- read_csv("Data/Raw data/Projections/wheat_hist_1990_2010.csv")
wheat_proj <- read_csv("Data/Raw data/Projections/wheat_ssp370_2040_2060.csv")

corn_hist <- corn_hist %>%
  pivot_wider(names_from = model, values_from = tas) %>%
  rename(province = iso) %>%
  mutate(province = sub("^IT-", "", province))

corn_proj <- corn_proj %>%
  pivot_wider(names_from = model, values_from = tas) %>%
  rename(province = iso) %>%
  mutate(province = sub("^IT-", "", province))

wheat_hist <- wheat_hist %>%
  pivot_wider(names_from = model, values_from = tas) %>%
  rename(province = iso) %>%
  mutate(province = sub("^IT-", "", province))

wheat_proj <- wheat_proj %>%
  pivot_wider(names_from = model, values_from = tas) %>%
  rename(province = iso) %>%
  mutate(province = sub("^IT-", "", province))

corn_hist <- corn_hist %>% mutate(ensemble = rowMeans(select(., -province), na.rm = TRUE))
corn_proj <- corn_proj %>% mutate(ensemble = rowMeans(select(., -province), na.rm = TRUE))
wheat_hist <- wheat_hist %>% mutate(ensemble = rowMeans(select(., -province), na.rm = TRUE))
wheat_proj <- wheat_proj %>% mutate(ensemble = rowMeans(select(., -province), na.rm = TRUE))

corn_all <- left_join(corn_hist, corn_proj, by = "province", suffix = c("_hist", "_proj"))
wheat_all <- left_join(wheat_hist, wheat_proj, by = "province", suffix = c("_hist", "_proj"))

# ---- CORN --------------------------------------------------------------------
d <- read.csv("Data/Raw data/RICA/synthetic_corn.csv")
d$mun_code <- as.character(d$mun_code)
d <- d %>% select(mun_code, province_code) %>% distinct()

temps <- readRDS("Data/Raw Data/Temperature/Corn monthly temperatures RICA unbiased.rds")
temps <- temps %>% ungroup()
temps$code <- as.character(temps$code)
temps <- temps %>% rename(mun_code = code, year = data_anno)
temps[, 3:14] <- round(temps[, 3:14], 2)

temps <- temps %>%
  mutate(mean_temp = rowMeans(select(., month3:month9), na.rm = TRUE)) %>%
  select(mun_code, year, mean_temp)

temps <- left_join(temps, d, by = c("mun_code"))
temps <- temps %>%
  group_by(province_code, year) %>%
  summarise(mean_temp = mean(mean_temp, na.rm = TRUE)) %>%
  ungroup()

temps <- temps %>%
  filter(year <= 2010) %>%
  group_by(province_code) %>%
  summarise(mean_temp_gs = mean(mean_temp, na.rm = TRUE)) %>%
  drop_na(province_code)

temps <- temps %>% rename(province = province_code)
t_corn <- temps %>% left_join(corn_all, by = c("province"))

corn_proj_diff <- t_corn %>%
  mutate(ensemble_unb       = mean_temp_gs + (ensemble_proj - ensemble_hist),
         cesm2_unb         = mean_temp_gs + (CESM2_proj - CESM2_hist),
         cmcc_cm2_sr5_unb  = mean_temp_gs + (`CMCC-CM2-SR5_proj` - `CMCC-CM2-SR5_hist`),
         cnrm_esm2_1_unb   = mean_temp_gs + (`CNRM-ESM2-1_proj` - `CNRM-ESM2-1_hist`),
         ec_earth3_veg_unb = mean_temp_gs + (`EC-Earth3-Veg_proj` - `EC-Earth3-Veg_hist`)
  )

saveRDS(corn_proj_diff, file = "Data/Processed data/Corn_projections_unbiased_ssp370.RDS")

# ---- WHEAT -------------------------------------------------------------------
d <- read.csv("Data/Raw data/RICA/synthetic_wheat.csv")
d$mun_code <- as.character(d$mun_code)
d <- d %>% select(mun_code, province_code) %>% distinct()

temps <- readRDS("Data/Raw data/Temperature/Wheat monthly temperatures RICA unbiased.rds")
temps$code <- as.character(temps$code)
temps <- temps %>% ungroup()
temps <- temps %>% rename(mun_code = code, year = data_anno)
temps[, 3:14] <- round(temps[, 3:14], 2)

# assign Oct-Dec to the next growing season
temps <- temps %>%
  pivot_longer(cols = starts_with("month"),
               names_to = "month",
               names_prefix = "month",
               values_to = "temperature")
temps <- temps %>%
  mutate(year = case_when(
    month %in% 1:9 ~ year,
    month %in% 10:12 ~ year + 1,
    TRUE ~ NA_real_
  ))
temps <- temps %>%
  pivot_wider(names_from = month,
              values_from = temperature,
              names_prefix = "month")

temps <- temps %>%
  mutate(mean_temp = rowMeans(select(., c(month1, month2, month3, month4,
                                          month5, month6, month7, month10,
                                          month11, month12)), na.rm = TRUE)) %>%
  select(mun_code, year, mean_temp)

temps <- left_join(temps, d, by = c("mun_code"))
temps <- temps %>%
  group_by(province_code, year) %>%
  summarise(mean_temp = mean(mean_temp, na.rm = TRUE)) %>%
  ungroup()

temps <- temps %>%
  filter(year <= 2010) %>%
  group_by(province_code) %>%
  summarise(mean_temp_gs = mean(mean_temp, na.rm = TRUE)) %>%
  drop_na(province_code)

temps <- temps %>% rename(province = province_code)
t_wheat <- temps %>% left_join(wheat_all, by = c("province"))

wheat_proj_diff <- t_wheat %>%
  mutate(ensemble_unb       = mean_temp_gs + (ensemble_proj - ensemble_hist),
         cesm2_unb         = mean_temp_gs + (CESM2_proj - CESM2_hist),
         cmcc_cm2_sr5_unb  = mean_temp_gs + (`CMCC-CM2-SR5_proj` - `CMCC-CM2-SR5_hist`),
         cnrm_esm2_1_unb   = mean_temp_gs + (`CNRM-ESM2-1_proj` - `CNRM-ESM2-1_hist`),
         ec_earth3_veg_unb = mean_temp_gs + (`EC-Earth3-Veg_proj` - `EC-Earth3-Veg_hist`)
  )

saveRDS(wheat_proj_diff, file = "Data/Processed data/Wheat_projections_unbiased_ssp370.RDS")

# ---- end of script -----------------------------------------------------------
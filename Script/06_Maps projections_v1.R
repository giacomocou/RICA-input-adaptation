### Script for CORN & WHEAT: Maps of projections ###############################

# ---- preliminary stuff -------------------------------------------------------
rm(list=ls())
gc()

# load packages
wants <- c("tidyverse", "readr", "readxl", "patchwork", "rnaturalearth", "rnaturalearthdata", "sf", "scico", "fixest", "conleyreg", "DescTools")
needs <- wants[!(wants %in% installed.packages()[,"Package"])]
if(length(needs)) install.packages(needs)
lapply(wants, function(i) require(i, character.only=TRUE))
rm(needs,wants)

# ---- load projection RDS files ------------------------------------------------

# Load CORN projections
file_paths_corn <- list.files(path = "Output/Corn/Projections", pattern = "\\.RDS$", full.names = TRUE)
for (fp in file_paths_corn) {
  fname <- basename(fp)
  model_name <- gsub("Corn_projections_ssp370_2050_|\\.RDS$", "", fname)
  df_name <- paste0("corn_", model_name)
  assign(df_name, readRDS(fp))
}

# Load WHEAT projections
file_paths_wheat <- list.files(path = "Output/Wheat/Projections", pattern = "\\.RDS$", full.names = TRUE)
for (fp in file_paths_wheat) {
  fname <- basename(fp)
  model_name <- gsub("Wheat_projections_ssp370_2050_|\\.RDS$", "", fname)
  df_name <- paste0("wheat_", model_name)
  assign(df_name, readRDS(fp))
}

# ---- calculate percentage changes to plot ------------------------------------

calculate_pct_change <- function(df) {
  df %>%
    mutate(
      y_2050_total = (y_2050_total / yield) * 100,
      y_2050_agro = (y_2050_agro / yield) * 100,
      y_2050_adaptation = (y_2050_adaptation / yield) * 100
    ) %>%
    select(province_code, province, scenario_used, y_2050_total, y_2050_agro, y_2050_adaptation)
}

input_names <- ls(pattern = "^(corn|wheat)_")
for (name in input_names) {
  df <- get(name)
  processed_df <- calculate_pct_change(df)
  new_name <- paste0("proj_", name)
  assign(new_name, processed_df)
}

# ---- load Italy province shapefiles ------------------------------------------

italy_provinces <- ne_states(geounit = "Italy", returnclass = "sf")
italy_provinces <- italy_provinces %>% rename(province_code = postal)

# ---- join ensemble projections to shapefile ----------------------------------

map_corn <- left_join(italy_provinces, proj_corn_ensemble_unb, by = "province_code") %>% mutate(crop = "Corn")
map_wheat <- left_join(italy_provinces, proj_wheat_ensemble_unb, by = "province_code") %>% mutate(crop = "Wheat")

map_all <- bind_rows(map_corn, map_wheat) %>% mutate(crop = factor(crop, levels = c("Corn", "Wheat")))

# ---- plot ensemble -----------------------------------------------------------

p <- ggplot(map_all) +
  geom_sf(aes(fill = y_2050_total), color = "gray70", size = 0.2) +
  facet_wrap(~ crop) +
  scale_fill_scico(
    palette = "vik",
    direction = -1,
    midpoint = 0,
    limits = c(-26, 4),
    name = "% Yield Change",
    na.value = "white"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 14),
    axis.text.x = element_text(size = 10, color = "darkgrey"),
    axis.text.y = element_text(size = 10, color = "darkgrey"),
    strip.text = element_text(hjust = 0.5, size = 20, color = "black")
  )

p
#ggsave(filename = "Output/Projections_ensemble_ssp370_map.tiff", p, width = 12, height = 7, dpi = 600, bg = "white", compression = "lzw")

# ---- join and plot individual model projections ------------------------------

corn_model_names <- ls(pattern = "^proj_corn_") %>% setdiff("proj_corn_ensemble_unb")
wheat_model_names <- ls(pattern = "^proj_wheat_") %>% setdiff("proj_wheat_ensemble_unb")

list_corn_sf <- lapply(corn_model_names, function(x) {
  italy_provinces %>%
    left_join(get(x), by = "province_code") %>%
    mutate(scenario_used = unique(na.omit(get(x)$scenario_used))) %>%
    mutate(scenario_clean = toupper(gsub("_unb", "", scenario_used)))
})
map_corn_4models <- do.call(rbind, list_corn_sf)

list_wheat_sf <- lapply(wheat_model_names, function(x) {
  italy_provinces %>%
    left_join(get(x), by = "province_code") %>%
    mutate(scenario_used = unique(na.omit(get(x)$scenario_used))) %>%
    mutate(scenario_clean = toupper(gsub("_unb", "", scenario_used)))
})
map_wheat_4models <- do.call(rbind, list_wheat_sf)

# ---- create plots for each crop ----------------------------------------------

p_corn <- ggplot(map_corn_4models) +
  geom_sf(aes(fill = y_2050_total), color = "gray70", size = 0.2) +
  facet_wrap(~ scenario_clean, ncol = 2) +
  scale_fill_scico(
    palette = "vik",
    direction = -1,
    midpoint = 0,
    limits = c(-26, 4),
    name = "% Yield Change",
    na.value = "white"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 14),
    axis.text.x = element_text(size = 10, color = "darkgrey"),
    axis.text.y = element_text(size = 10, color = "darkgrey"),
    strip.text = element_text(hjust = 0.5, size = 20, color = "black")
  )
p_corn

p_wheat <- ggplot(map_wheat_4models) +
  geom_sf(aes(fill = y_2050_total), color = "gray70", size = 0.2) +
  facet_wrap(~ scenario_clean, ncol = 2) +
  scale_fill_scico(
    palette = "vik",
    direction = -1,
    midpoint = 0,
    limits = c(-26, 4),
    name = "% Yield Change",
    na.value = "white"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 14),
    axis.text.x = element_text(size = 10, color = "darkgrey"),
    axis.text.y = element_text(size = 10, color = "darkgrey"),
    strip.text = element_text(hjust = 0.5, size = 20, color = "black")
  )
p_wheat

# ---- save plots --------------------------------------------------------------

#ggsave(filename = "Output/Corn_4models_ssp370_map.tiff", p_corn, width = 12, height = 12, dpi = 600, bg = "white", compression = "lzw")
#ggsave(filename = "Output/Wheat_4models_ssp370_map.tiff", p_wheat, width = 12, height = 12, dpi = 600, bg = "white", compression = "lzw")
# This script generates synthetic farm-level data for corn and wheat,
# replicating the structure and statistical properties of the RICA dataset
# (Table 1 in the paper). The generated datasets include geographic information
# of real municipalities, area cultivated, input usage (fertiliser and
# pesticide), and output quantity. The data is designed to allow for replication
# of analyses without requiring access to the original RICA data, while
# maintaining realistic distributions and relationships between variables. This
# data doesn't assume any correlation structure with weather variables, so it is
# possible that the estimation does not converge.

# CORN #########################################################################

set.seed(123)

n_farms <- 500
years <- 2008:2022

# --- Municipality coordinates ---
coords <- read.csv("Data/Raw Data/Coordinates/Coordinates_corn.csv")

# --- Farm-level fixed attributes ---
farm_ids   <- paste0("FARM_", sprintf("%04d", 1:n_farms))
mun_idx    <- sample(nrow(coords), n_farms, replace = TRUE)
mun_sample <- coords[mun_idx, ]

# --- Farm-level baseline area ---
base_area <- rlnorm(n_farms, meanlog = log(20) - 0.5 * log(1 + (28/20)^2),
                    sdlog  = sqrt(log(1 + (28/20)^2)))
base_area <- pmin(base_area, 400)  # cap at 400

# --- Farm-level unobserved heterogeneity (fixed effect) ---
farm_effect <- rnorm(n_farms, mean = 0, sd = 15)

# ============================================================
# CORN
# ============================================================
rows <- list()
for (i in 1:n_farms) {
  n_years <- sample(3:10, 1)
  
  # unbalanced panel. 80% of farms have consecutive years, 20% have gaps
  if (runif(1) < 0.8) {
    start_year <- sample(years[1]:(tail(years, 1) - n_years + 1), 1)
    yrs <- start_year:(start_year + n_years - 1)
  } else {
    yrs <- sort(sample(years, n_years))
  }
  
  for (y in yrs) {
    # Year-to-year variation around farm baseline
    area <- round(pmax(0.5, base_area[i] + rnorm(1, sd = base_area[i] * 0.1)), 2)
    
    # Log-normal per hectare, capped at realistic maxima
    fert_per_ha <- min(950, rlnorm(1, meanlog = log(258) - 0.5 * log(1 + (148/258)^2),
                                   sdlog  = sqrt(log(1 + (148/258)^2))))
    pest_per_ha <- min(500, rlnorm(1, meanlog = log(126) - 0.5 * log(1 + (77/126)^2),
                                   sdlog  = sqrt(log(1 + (77/126)^2))))
    
    fertiliser <- round(area * fert_per_ha)
    pesticide  <- round(area * pest_per_ha)
    
    # DGP: yield per ha depends on inputs per ha + farm effect + noise
    yield_per_ha <- 50 +
      0.2 * fert_per_ha +          # fertiliser effect
      0.1 * pest_per_ha +          # pesticide effect
      farm_effect[i] +             # farm fixed effect
      rnorm(1, sd = 15)            # idiosyncratic shock
    
    quantity <- round(max(0, area * yield_per_ha))
    
    rows[[length(rows) + 1]] <- data.frame(
      farm_code     = farm_ids[i],
      year          = y,
      mun_code      = mun_sample$mun_code[i],
      province      = mun_sample$province[i],
      province_code = mun_sample$province_code[i],
      region        = mun_sample$region[i],
      agr_reg_code  = mun_sample$agr_reg_code[i],
      longitude     = mun_sample$longitude[i],
      latitude      = mun_sample$latitude[i],
      area          = area,
      fertiliser    = fertiliser,
      pesticide     = pesticide,
      quantity      = quantity
    )
  }
}

corn <- do.call(rbind, rows)

# --- Save ---
write.csv(corn, "Data/Raw data/RICA/synthetic_corn.csv", row.names = FALSE)

cat("Corn:", nrow(corn), "rows,", length(unique(corn$farm_code)), "farms\n")

# WHEAT ########################################################################

set.seed(456)

n_farms <- 800
years <- 2008:2022

# --- Municipality coordinates ---
coords_w <- read.csv("Data/Raw Data/Coordinates/Coordinates_wheat.csv")

# --- Farm-level fixed attributes ---
farm_ids   <- paste0("FARM_", sprintf("%04d", 1:n_farms))
mun_idx    <- sample(nrow(coords_w), n_farms, replace = TRUE)
mun_sample <- coords_w[mun_idx, ]

# --- Farm-level baseline area ---
base_area <- rlnorm(n_farms, meanlog = log(17) - 0.5 * log(1 + (25/17)^2),
                    sdlog  = sqrt(log(1 + (25/17)^2)))
base_area <- pmin(base_area, 400)  # cap at 400

# --- Farm-level unobserved heterogeneity (fixed effect) ---
farm_effect <- rnorm(n_farms, mean = 0, sd = 10)

# ============================================================
# WHEAT
# ============================================================
rows <- list()
for (i in 1:n_farms) {
  n_years <- sample(3:10, 1)
  
  # unbalanced panel. 80% of farms have consecutive years, 20% have gaps
  if (runif(1) < 0.8) {
    start_year <- sample(years[1]:(tail(years, 1) - n_years + 1), 1)
    yrs <- start_year:(start_year + n_years - 1)
  } else {
    yrs <- sort(sample(years, n_years))
  }
  
  for (y in yrs) {
    # Year-to-year variation around farm baseline
    area <- round(pmax(0.5, base_area[i] + rnorm(1, sd = base_area[i] * 0.1)), 2)
    
    # Log-normal per hectare, capped at realistic maxima
    fert_per_ha <- min(608, rlnorm(1, meanlog = log(144) - 0.5 * log(1 + (87/144)^2),
                                   sdlog  = sqrt(log(1 + (87/144)^2))))
    pest_per_ha <- min(370, rlnorm(1, meanlog = log(88) - 0.5 * log(1 + (60/88)^2),
                                   sdlog  = sqrt(log(1 + (60/88)^2))))
    
    fertiliser <- round(area * fert_per_ha)
    pesticide  <- round(area * pest_per_ha)
    
    # DGP: yield per ha depends on inputs per ha + farm effect + noise
    yield_per_ha <- 30 +
      0.15 * fert_per_ha +         # fertiliser effect
      0.08 * pest_per_ha +         # pesticide effect
      farm_effect[i] +             # farm fixed effect
      rnorm(1, sd = 10)            # idiosyncratic shock
    
    quantity <- round(max(0, area * yield_per_ha))
    
    rows[[length(rows) + 1]] <- data.frame(
      farm_code     = farm_ids[i],
      year          = y,
      mun_code      = mun_sample$mun_code[i],
      province      = mun_sample$province[i],
      province_code = mun_sample$province_code[i],
      region        = mun_sample$region[i],
      agr_reg_code  = mun_sample$agr_reg_code[i],
      longitude     = mun_sample$longitude[i],
      latitude      = mun_sample$latitude[i],
      area          = area,
      fertiliser    = fertiliser,
      pesticide     = pesticide,
      quantity      = quantity
    )
  }
}

wheat <- do.call(rbind, rows)

# --- Save ---
write.csv(wheat, "Data/Raw data/RICA/synthetic_wheat.csv", row.names = FALSE)

cat("Wheat:", nrow(wheat), "rows,", length(unique(wheat$farm_code)), "farms\n")

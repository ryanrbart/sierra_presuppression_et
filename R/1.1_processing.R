# Process Kings Watershed Data
# 
# Outputs
# Tibble: db_final_year
# Tibble: db_final_all

source("R/0_utilities.R")

# ---------------------------------------------------------------------
# Assign NDVI change variables
# Run 0_establish_lai_ndvi_relation.R to establish A and B
source("R/0_establish_lai_ndvi_relation.R")

lai_change <- 0.5           # New level of biomass/lai as fraction of original biomass/lai (e.g. 30% reduction in lai would be 0.7)
ndvi_change <- ndvi_adjust(lai_change = lai_change, A = A, B = B)

# ---------------------------------------------------------------------
# Import data

ndvi <- read_csv("data/ndvi_by_elevation.csv")
precip <- read_csv("data/precip_by_elevation.csv")
tmax <- read_csv("data/tmax_by_elevation.csv")
q <- read_csv("data/q_by_basin.csv")


# There is a minor inconsistancy between the number_cells column in the ndvi
# file compared to the precip and tmax file. Adjusting the NDVI file to be
# consistent.
ndvi$number_cells[34] <- 2854    # Original 2853
ndvi$number_cells[35] <- 1963    # Original 1959
ndvi$number_cells[36] <- 1197    # Original 1196
ndvi$number_cells[37] <- 591     # Original 588

# ---------------------------------------------------------------------
# Process data

# Process NDVI
ndvi_tmp <- ndvi %>% 
  # Gather by wateryear
  tidyr::gather(-elevation_m, -number_cells, key=year, value=current) %>% 
  # Reduce ndvi by appropriate amount
  dplyr::mutate(historical = current+ndvi_change) %>% 
  # Gather current and historical periods
  tidyr::gather(-elevation_m, -number_cells, -year, key=period, value=ndvi) %>% 
  # Calculate ET
# dplyr::mutate(et = 123.8243*exp(2.5456*ndvi))         # Roche2018 for Landsat
# dplyr::mutate(et_vpsat = 3.7129*exp(3.0748*ndvi))     # Bales2018 for MODIS
# dplyr::mutate(et = 10.3247*exp(2.8599*ndvi))           # Goulden2014 for MODIS
  dplyr::mutate(et = 101.49*exp(2.6853*ndvi))           # Goulden2012 for MODIS



# Process Precipitation
precip_tmp <- precip %>% 
  # Gather by wateryear
  tidyr::gather(-elevation_m, -number_cells, key=year, value=precip)


# Process Tmax
tmax_tmp <- tmax %>% 
  # Gather by wateryear
  tidyr::gather(-elevation_m, -number_cells, key=year, value=tmax)


# Bind NDVI and ET with Precip and then Tmax
db_tmp <- dplyr::full_join(ndvi_tmp, precip_tmp, by=c("elevation_m", "number_cells", "year"))
db_tmp <- dplyr::full_join(db_tmp, tmax_tmp, by=c("elevation_m", "number_cells", "year"))
#View(db_final)


# Compute P - ET
db_tmp <- db_tmp %>% 
  dplyr::mutate(p_minus_et = precip - et)


# Compute weighted averaged P, ET, P_minus_ET values by elevation band and by year
db_final_year <- db_tmp %>% 
  dplyr::mutate(precip_total_by_year = number_cells*precip,
                et_total_by_year = number_cells*et,
                p_minus_et_total_by_year = number_cells*p_minus_et) %>% 
  dplyr::group_by(period, year) %>% 
  dplyr::mutate(precip_percent_by_year = (precip_total_by_year/sum(precip_total_by_year))*100,
                et_percent_by_year = (et_total_by_year/sum(et_total_by_year))*100,
                p_minus_et_percent_by_year = (p_minus_et_total_by_year/sum(p_minus_et_total_by_year))*100,
                area_fraction = (number_cells/sum(number_cells))*100) %>% 
  dplyr::ungroup()

#View(db_final_year)


# Compute weighted averaged P, ET, P_minus_ET values by elevation band and all years
db_final_all <- db_tmp %>% 
  # Average across years
  dplyr::group_by(elevation_m,number_cells,period) %>% 
  dplyr::summarise(et = mean(et),
                   precip = mean(precip),
                   p_minus_et = mean(p_minus_et)) %>% 
  dplyr::mutate(precip_total_all_year = number_cells*precip,
                et_total_all_year = number_cells*et,
                p_minus_et_total_all_year = number_cells*p_minus_et) %>% 
  dplyr::group_by(period) %>% 
  dplyr::mutate(precip_percent_all_year = (precip_total_all_year/sum(precip_total_all_year))*100,
                et_percent_all_year = (et_total_all_year/sum(et_total_all_year))*100,
                p_minus_et_percent_all_year = (p_minus_et_total_all_year/sum(p_minus_et_total_all_year))*100,
                area_fraction = (number_cells/sum(number_cells))*100) %>% 
  dplyr::ungroup()

#View(db_final_all)


write_csv(db_final_year, "output/output_1/db_final_year.csv")
write_csv(db_final_all, "output/output_1/db_final_all.csv")



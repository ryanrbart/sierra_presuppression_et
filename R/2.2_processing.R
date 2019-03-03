# Process Kings Watershed Data
# 
# Outputs
# Tibble: db_final_year
# Tibble: db_final_all

source("R/0_utilities.R")
source("R/2.1_exploratory_snamp_data.R")

# ---------------------------------------------------------------------
# Assign NDVI change variables

# Run 0_establish_lai_ndvi_relation.R to establish A and B
cc_change <- 0.5           # New level of canopy cover as fraction of original canopy cover (e.g. 30% reduction in lai would be 0.7)

# ---------------------------------------------------------------------
# Import data for Kings Watershed

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
  #dplyr::mutate(historical = cc_ndvi_log(current,cc_change,a3,b3)) %>% 
  #dplyr::mutate(historical = cc_ndvi_power(current,cc_change,a4,b4)) %>% 
  dplyr::mutate(historical = case_when(
    elevation_m < 800 ~ cc_ndvi_power(ndvi_start=current,cc_reduction=1,a=a4,b=b4),
    elevation_m >= 800 & elevation_m < 900 ~ cc_ndvi_power(ndvi_start=current,cc_reduction=0.8,a=a4,b=b4),
    elevation_m >= 900 & elevation_m < 1000 ~ cc_ndvi_power(ndvi_start=current,cc_reduction=0.7,a=a4,b=b4),
    elevation_m >= 1000 & elevation_m < 1200 ~ cc_ndvi_power(ndvi_start=current,cc_reduction=0.6,a=a4,b=b4),
    elevation_m >= 1200 & elevation_m < 3000 ~ cc_ndvi_power(ndvi_start=current,cc_reduction=0.5,a=a4,b=b4),
    elevation_m >= 3000 & elevation_m < 3100 ~ cc_ndvi_power(ndvi_start=current,cc_reduction=0.75,a=a4,b=b4),
    elevation_m >= 3100 ~ cc_ndvi_power(ndvi_start=current,cc_reduction=1,a=a4,b=b4))) %>%
  # Gather current and historical periods
  tidyr::gather(-elevation_m, -number_cells, -year, key=period, value=ndvi) %>% 
  # Calculate ET
  # dplyr::mutate(et = 123.8243*exp(2.5456*ndvi))         # Roche2018 for Landsat
  # dplyr::mutate(et = 3.7129*exp(3.0748*ndvi)*satVPweighted)     # Bales2018 for MODIS
  dplyr::mutate(et = 3.7129*exp(3.0748*ndvi)*23.52738)     # Bales2018 for MODIS (satvp value from average satvp value on 'ET regression' tab)
# dplyr::mutate(et = 10.3247*exp(2.8599*ndvi))           # Goulden2014 for MODIS
# dplyr::mutate(et = 101.49*exp(2.6853*ndvi))           # Goulden2012 for MODIS



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




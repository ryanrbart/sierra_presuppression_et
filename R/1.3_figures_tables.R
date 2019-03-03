# Figures and Tables
# 

source("R/0_utilities.R")

# ---------------------------------------------------------------------
# Load processed tables

db_final_year <- read_csv("output/output_1/db_final_year.csv")
db_final_all <- read_csv("output/output_1/db_final_all.csv")


# ---------------------------------------------------------------------
# Plot Precip, ET and P_minus_ET by elevation

# Change in water fluxes - Single year

x <- db_final_year %>% 
  #tidyr::gather(key = "flux", value = "value", et, precip, p_minus_et) %>% 
  dplyr::filter(year=='WY 2001') %>% 
  ggplot() +
  geom_point(aes(x=elevation_m, y=et, shape = period, color = "ET")) + 
  geom_point(aes(x=elevation_m, y=precip, shape = period, color = "Precipitation")) +
  geom_point(aes(x=elevation_m, y=p_minus_et, shape = period, color = "P-ET")) + 
  labs(x = "Elevation (m)", y = expression('Water flux (mm yr'^-1*')')) +
  geom_hline(aes(yintercept=0)) +
  scale_colour_manual(name="Fluxes",
                      values=c(ET="green4", 
                               Precipitation="black", 
                               'P-ET'="blue")) +
  scale_shape_manual(values=c(19, 3),
                     labels = c("Modern", "Historical"),
                     name = "Period") +
  theme_set(theme_bw(base_size = 12)) +
  NULL
plot(x)
ggsave("water_fluxes_2001.pdf", plot = x, device = "pdf", path = "output/output_1", width = 8, height = 7)


# Change in water fluxes - All years separated
x <- ggplot(db_final_year) +
  geom_point(aes(x=elevation_m, y=et, shape = period, color = "ET")) + 
  geom_point(aes(x=elevation_m, y=precip, shape = period, color = "Precipitation")) +
  geom_point(aes(x=elevation_m, y=p_minus_et, shape = period, color = "P-ET")) + 
  geom_hline(aes(yintercept=0)) +
  labs(x = "Elevation (m)", y = expression('Water flux (mm yr'^-1*')')) +
  facet_wrap(~year) +
  scale_colour_manual(name="Fluxes",
                      values=c(ET="green4", 
                               Precipitation="black", 
                               'P-ET'="blue")) +
  scale_shape_manual(values=c(19, 3),
                     labels = c("Modern", "Historical"),
                     name = "Period") +
  theme_set(theme_bw(base_size = 11)) +
  NULL
plot(x)
ggsave("water_fluxes_year.pdf", plot = x, device = "pdf", path = "output/output_1", width = 8, height = 5.5)



# Change in water fluxes - All years combined
x <- db_final_all %>% 
  ggplot(.) +
  geom_point(aes(x=elevation_m, y=et, shape = period, color = "ET"), size=2.2) + 
  geom_line(aes(x=elevation_m, y=et, group = period, color = "ET")) + 
  geom_point(aes(x=elevation_m, y=precip, color = "Precipitation"), size=2.2) +
  geom_line(aes(x=elevation_m, y=precip, group = period, color = "Precipitation")) +
  geom_point(aes(x=elevation_m, y=p_minus_et, shape = period, color = "P-ET"), size=2.2) +
  geom_line(aes(x=elevation_m, y=p_minus_et, group = period, color = "P-ET")) +
  labs(x = "Elevation (m)", y = expression('Water flux (mm yr'^-1*')')) +
  scale_colour_manual(name="Fluxes (mm)",
                      values=c(ET="green4", 
                               Precipitation="black", 
                               'P-ET'="blue")) +
  scale_shape_manual(values=c(19, 17),
                     labels = c("Modern", "Historical"),
                     name = "Period") +
  theme_set(theme_bw(base_size = 14)) +
  ylim(0,NA) +
  NULL
plot(x)
ggsave("water_fluxes_all.pdf", plot = x, device = "pdf", path = "output/output_1", width = 7, height = 4)


# Largest difference for a given bin
db_final_all %>% 
  dplyr::select(elevation_m, et) %>% 
  dplyr::group_by(elevation_m) %>% 
  dplyr::summarise(et_diff = -diff(et)) %>% 
  dplyr::summarise(et_max = max(et_diff))

# ---------------------------------------------------------------------
# Plot Area weighted P_minus_ET

# Plot p_minus_et as percent
x <- db_final_all %>% 
  ggplot(.) +
  geom_col(aes(x=elevation_m, y = area_fraction), 
               position = position_dodge()) +
  geom_line(aes(x=elevation_m, y = p_minus_et_percent_all_year, linetype=period, color=period),size=1) +
  geom_point(aes(x=elevation_m, y = p_minus_et_percent_all_year, shape=period, color=period), size=2.5) +
  labs(x = "Elevation (m)", y = "Fraction contributed by 100m bin") +
  scale_color_brewer(name="Period",
                     palette = "Set1",
                     labels = c("Modern", "Historical")) +
  scale_shape_discrete(name="Period",
                       labels = c("Modern", "Historical")) +
  scale_linetype_discrete(name="Period",
                       labels = c("Modern", "Historical")) +
  theme_set(theme_bw(base_size = 14)) +
  NULL
plot(x)
ggsave("area_weighted_fluxes2.pdf", plot = x, device = "pdf", path = "output/output_1", width = 7, height = 4)


# Plot p_minus_et as percent of historical
x <- db_final_all %>% 
  dplyr::select(elevation_m, number_cells, period, p_minus_et_total_all_year, area_fraction) %>% 
  tidyr::spread(period, p_minus_et_total_all_year) %>% 
  dplyr::mutate(current_percent = (current/sum(historical))*100,              # current as a percent of historical
                historical_percent = (historical/sum(historical))*100) %>%    # historical as a percent of historical
  tidyr::gather(current_percent, historical_percent,
                key = 'period', value = p_minus_et_percent) %>% 
  ggplot(.) +
  geom_col(aes(x=elevation_m, y = area_fraction), 
           position = position_dodge(), fill="gray75") +
  geom_line(aes(x=elevation_m, y = p_minus_et_percent, linetype=period, color=period),size=1) +
  geom_point(aes(x=elevation_m, y = p_minus_et_percent, shape=period, color=period), size=2.5) +
  labs(x = "Elevation (m)", y = "Fraction contributed by 100m bin") +
  scale_color_brewer(name="Period",
                     palette = "Set1",
                     labels = c("Modern", "Historical")) +
  scale_shape_discrete(name="Period",
                       labels = c("Modern", "Historical")) +
  scale_linetype_discrete(name="Period",
                          labels = c("Modern", "Historical")) +
  theme_set(theme_bw(base_size = 14)) +
  NULL
plot(x)
ggsave("area_weighted_fluxes.pdf", plot = x, device = "pdf", path = "output/output_1", width = 7, height = 4)




# ---------------------------------------------------------------------
# Summary Tables

# Watershed ET and P_minus_ET totals by year
summary_year <- db_final_year %>% 
  dplyr::group_by(year, period) %>% 
  dplyr::summarise(precip = mean(precip_total_by_year/mean(number_cells)),
                   et = mean(et_total_by_year/mean(number_cells)),
                   p_minus_et = mean(p_minus_et_total_by_year/mean(number_cells))) %>% 
  ungroup() 

# Compare changes in ET from year to year
summary_year %>% 
  dplyr::select(-c(precip, p_minus_et)) %>% 
  tidyr::spread(period, et) %>% 
  dplyr::mutate(et_diff = historical - current)



# Watershed ET and P_minus_ET totals across years
summary_all <- db_final_all %>% 
  dplyr::group_by(period) %>% 
  dplyr::summarise(precip = mean(precip_total_all_year/mean(number_cells)),
                   et = mean(et_total_all_year/mean(number_cells)),
                   p_minus_et = mean(p_minus_et_total_all_year/mean(number_cells)))


# ---------------------------------------------------------------------
# Calculate mean full natural flow

q_annual <- q %>% 
  dplyr::mutate(flow_mm = flow_af/3998*1.233) %>% 
  dplyr::group_by(wy) %>% 
  dplyr::summarise(flow_mm = sum(flow_mm))


# Average streamflow from 2001 to 2016
q_annual %>% 
  dplyr::filter(wy>2000, wy<2017) %>% 
  dplyr::summarise(mean_flow = mean(flow_mm))












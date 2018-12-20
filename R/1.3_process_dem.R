# Process DEM for Kings Watershed ET map

source("R/0_utilities.R")

# ---------------------------------------------------------------------
# Define projections

proj_longlat <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

# ---------------------------------------------------------------------
# Get data

# Load DEM
krb_dem <- raster::raster("../KRB_GIS/dem/KRB.tif")
# Reproject dem
krb_dem_proj <- projectRaster(krb_dem, crs = proj_longlat)

# Load Kings boundary
kings_border <- st_read(dsn = "data/gis/", layer="kings")
kings_border <- st_transform(kings_border, crs = proj_longlat)
kings_border_sp <- as(st_geometry(kings_border), "Spatial")  # Change from sf to sp object since raster::crop won't work with extent of sf object
e <- extent(kings_border_sp)
krb_dem_c <- raster::crop(krb_dem_proj, (e+0.05))

# Null out raster areas outside of kings border
kings_border_rast <- rasterize(kings_border_sp, krb_dem_c)
krb_dem_c[is.na(kings_border_rast) == TRUE] <- NA
plot(krb_dem_c)
plot(kings_border_sp, add=TRUE)


# Load processed tables
db_final_year <- read_csv("output/output_1/db_final_year.csv")
db_final_all <- read_csv("output/output_1/db_final_all.csv")


# ---------------------------------------------------------------------
# Prep bins 

# Generate difference in ET
et_diff <- db_final_all %>% 
  dplyr::select(c(elevation_m, et, period)) %>% 
  tidyr::spread(period, et) %>% 
  dplyr::mutate(et_diff = historical - current)

# Generate difference in P_minus_ET
p_minus_et_diff <- db_final_all %>% 
  dplyr::select(c(elevation_m, p_minus_et, period)) %>% 
  tidyr::spread(period, p_minus_et) %>% 
  dplyr::mutate(p_minus_et_diff = historical - current)

# Join differences
flux_bins <- dplyr::full_join(et_diff, p_minus_et_diff, by = c("elevation_m"))


# Add dummy rows if using full dem extent
# flux_bins_extra_low <- flux_bins[1:2,]
# flux_bins_extra_low$elevation_m <- c(50,150)
# flux_bins_extra_low$p_minus_et_diff <- c(40,40)
# flux_bins_extra_high <- flux_bins[1:2,]
# flux_bins_extra_high$elevation_m <- c(4350,4450)
# flux_bins_extra_high$p_minus_et_diff <- c(24,24)
# flux_bins <- dplyr::bind_rows(flux_bins_extra_low, flux_bins, flux_bins_extra_high)
flux_bins_extra_high <- flux_bins[1,]
flux_bins_extra_high$elevation_m <- c(4350)
flux_bins_extra_high$p_minus_et_diff <- c(24)
flux_bins <- dplyr::bind_rows(flux_bins, flux_bins_extra_high)

# ---------------------------------------------------------------------
# Create raster showing ET and P_minus_ET change between current and historical conditions.

r1 <- krb_dem_c
for (aa in seq_len(nrow(flux_bins))){
  value <- flux_bins[aa,]$p_minus_et_diff
  elev <- flux_bins[aa,]$elevation_m

  r1 <- raster::calc(r1, function(x){x[x>(elev-50) & x<=(elev+50)] <- value; return(x)})
}

# print(r1)
# plot(r1)

# ---------------------------------------------------------------------
# Convert to tibble

flux_delta <- r1 %>% 
  rasterToPoints() %>% 
  as_tibble()

x <- ggplot() +
  geom_raster(data=flux_delta,aes(x=x,y=y, fill=layer)) +
  geom_sf(data=kings_border, fill=NA, col="black") +
  scale_fill_continuous(low="red", high="blue", name="Reduction in\nET (mm)") +
  #scale_x_continuous(expand=c(0,0)) +   # This eliminates margin buffer around plot
  #scale_y_continuous(expand=c(0,0)) +   # This eliminates margin buffer around plot
  labs(title=paste("Reduction in historical ET relative to present day in Kings River Watershed"), x="Longitude",y="Latitude", size=0.5) +
  theme_classic(base_size =12) +
  theme (panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "grey"))  +
  NULL
plot(x)

ggsave(paste("output/output_1/map_historical_ET_diff.jpg",sep=""),plot=x, width = 8, height = 7.8)







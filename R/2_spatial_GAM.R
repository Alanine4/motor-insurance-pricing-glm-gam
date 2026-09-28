# 2_spatial_GAM.R
# Smooth spatial effect on claim frequency, fitted per postal code and mapped
# over Belgium (requires df from 0_setup_and_EDA.R).

library(sf)
library(dplyr)
library(mgcv)
library(ggplot2)

# using shapefile of Belgium
bel_shp <- st_read("data/shapefile/npc96_region_Project1.shp", quiet = TRUE) %>%
  st_transform(4326)

post_expo <- df %>%
  mutate(codposs = as.character(codposs)) %>%
  group_by(codposs) %>%
  summarize(
    total_expo = sum(expo, na.rm = TRUE),
    nclaims    = sum(nclaims, na.rm = TRUE),
    .groups     = "drop"
  )


sf::sf_use_s2(FALSE)
centroids <- st_centroid(bel_shp)

# export POSTCODE to newdata_all
newdata_all <- centroids %>%
  transmute(
    POSTCODE  = as.character(POSTCODE),
    longitude = st_coordinates(geometry)[,1],
    latitude  = st_coordinates(geometry)[,2]
  ) %>%
  
  left_join(post_expo, by = c("POSTCODE"="codposs")) %>%
  mutate(
    total_expo = ifelse(is.na(total_expo), 1, total_expo),
    nclaims    = ifelse(is.na(nclaims),    0, nclaims)
  )

#  using GAM to predict
freq_gam_spatial <- gam(
  nclaims ~ s(longitude, latitude, bs = "tp"),
  offset    = log(total_expo),
  family    = poisson(link = "log"),
  data      = newdata_all
)


newdata_all <- newdata_all %>%
  mutate(fit_spatial = predict(
    freq_gam_spatial,
    newdata = newdata_all,
    type    = "response"
  ))



newdata_df <- newdata_all %>% 
  st_drop_geometry()    

# POSTCODE left_join
bel_shp2 <- bel_shp %>%
  mutate(POSTCODE = as.character(POSTCODE)) %>%
  left_join(
    newdata_df %>% select(POSTCODE, fit_spatial),
    by = "POSTCODE"
  )


glimpse(bel_shp2)


# the entire map of Belgium with extrapolation
spatial_map <- ggplot(bel_shp2) +
  geom_sf(aes(fill = fit_spatial), colour = NA) +
  scale_fill_gradient(low = "#99CCFF", high = "#003366", na.value = "white") +
  labs(
    title = "Predicted Claim Frequency",
    fill  = "Predicted\nfrequency"
  ) +
  theme_bw()
print(spatial_map)
ggsave("figures/spatial_gam_frequency.png", spatial_map, width = 7, height = 6, dpi = 150)

# 1_spatial_EDA.R
# Exposure per unit area by Belgian postal code (requires df from 0_setup_and_EDA.R).

## ----------------------------- Setup ---------------------------------------
library(readr)
library(readxl)
library(dplyr)
library(sf)
library(ggplot2)
library(tmap)

# -- 1. Prepare the policy data
df <- df %>% 
  rename_all(tolower) %>% 
  mutate(
    codposs  = as.character(codposs),   # convert postal code to character
    expo     = as.numeric(expo),        # exposure
    nbrtotc  = as.numeric(nclaims),     # claim count
    chargtot = as.numeric(nbrtotan)     # total claim amount
  )

# -- 2. Read and transform the Belgium shapefile
belgium_shape_sf <- st_read("data/shapefile/npc96_region_Project1.shp", quiet = TRUE) %>% 
  st_transform(crs = 4326)             # reproject to WGS84

# inspect to find the postal code field name in the shapefile
print(names(belgium_shape_sf))

# ----------------------------------------------------------------------------
# -- 3. Build post_expo summary from your df
post_expo <- df %>%
  group_by(codposs) %>%
  summarize(
    num        = n(),                  # count of records per postal code
    total_expo = sum(expo, na.rm = TRUE)  # total exposure per postal code
  )

post_expo %>% slice(1:5)

# ----------------------------------------------------------------------------
# -- 4. Join the exposure summary back to the sf object
# 1. ensure shapefile's POSTCODE is character
belgium_shape_sf <- belgium_shape_sf %>%
  mutate(POSTCODE = as.character(POSTCODE))

# 2. ensure post_expo codposs is character
post_expo <- post_expo %>%
  mutate(codposs = as.character(codposs))

# 3. perform left join by matching shapefile POSTCODE to df codposs
belgium_shape_sf <- belgium_shape_sf %>%
  left_join(post_expo, by = c("POSTCODE" = "codposs"))

# 4. inspect the merged result
glimpse(belgium_shape_sf)

# ----------------------------------------------------------------------------
# -- 5. Compute exposure per unit area and classify into three levels
belgium_shape_sf <- belgium_shape_sf %>%
  mutate(
    freq = total_expo / Shape_Area,
    freq_class = cut(
      freq,
      breaks = quantile(freq, c(0, 0.2, 0.8, 1), na.rm = TRUE),
      right = FALSE, include.lowest = TRUE,
      labels = c("low", "average", "high")
    )
  )

# ----------------------------------------------------------------------------
# -- 6. Plot: exposure density class by postal code area
expo_map <- ggplot(belgium_shape_sf) +
  geom_sf(aes(fill = freq_class),
          colour = "black", size = 0.1) +
  scale_fill_brewer(palette = "Blues", na.value = "white") +
  labs(
    title = "Exposure Density by Postal Code Area",
    fill  = "Exposure\nClass"
  ) +
  theme_bw()
print(expo_map)
ggsave("figures/exposure_density_map.png", expo_map, width = 7, height = 6, dpi = 150)


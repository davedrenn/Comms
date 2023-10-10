# Exploratory analysis and visualization of data contained within the {zipcodeR} package for the paper
# "zipcodeR: Advancing the analysis of spatial data at the ZIP code level in R" submitted to Software Impacts

library(zipcodeR)
library(tidyverse)
library(sf)
library(ggmap)
library(tabplot)

# Obtain data from zipcodeR
data("zip_code_db")

zip_code_db <- zip_code_db %>%
  mutate(Region = substr(zipcode, 1, 1))

# Filter ZIP codes suitable for mapping w/ valid data
geo <- zip_code_db %>%
  filter(lat != is.na(.) & zipcode_type == "Standard")

# Subset coordinates to be mapped
coords <- geo %>%
  mutate(Region = substr(zipcode, 1, 1)) %>%
  filter(state %in% c("AK", "HI", "PR") == FALSE) %>%
  select(zipcode, Region, lat, lng)

# Figure 1 - Map of U.S. ZIP Code centroids contained within zip_code_db colorized by region
fig1 <- qmplot(lng, lat,
  data = coords, maptype = "toner-lite", group = Region,
  color = Region, main = "Continental U.S. ZIP Code Centroids by Region",
  size = I(0.01), geom = "auto", padding = 0.02, darken = .7, zoom = 6
) +
  guides(color = guide_legend(override.aes = list(size = 3)))

# View and save the figure
ggsave("fig1.png",fig1)

# Figure 2 - Tableplot
# Remove blob columns for compatibility with tabplot
zip_subset <- zip_code_db %>%
  select(-common_city_list, -area_code_list) %>%
  rename("region" = Region)

zip_subset <- tablePrepare(zip_subset)

fig2 <- tableplot(zip_subset,
  select = c(population, zipcode_type, region, median_home_value),
  scales = "auto", numMode = "mb-sdb-sdl", pals = list("Paired"),
  title = "Distribution of U.S. ZIP Codes in zip_code_db by population",
  fontsize = 18, fontsize.title = 30
)

# Save the figure
tableSave(fig2, filename = "Figure2.tiff", width = 12, height = 8)

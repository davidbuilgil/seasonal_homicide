
rm(list = ls())
options(scipen=999)

# Load packages
library(here)
library(ggplot2)
library(plm)
library(nlme)
library(stringr)
library(ggpubr)
library(lubridate)
library(dplyr)

# Load precipitation data
rainfall1 <- read.csv(here("data/col-rainfall-adm2-part1.csv"))
rainfall2 <- read.csv(here("data/col-rainfall-adm2-part2.csv"))
rainfall3 <- read.csv(here("data/col-rainfall-adm2-part3.csv"))
rainfall <- rainfall1 %>%
  rbind(rainfall2, rainfall3)

rm(rainfall1, rainfall2, rainfall3)

# Find month and year in date
rainfall <- rainfall %>%
  mutate(year = year(date),
         month = month(date))

# Aggregate data monthly
rainfall_month <- rainfall %>%
  group_by(month, year) %>%
  summarise(rfh = sum(rfh)) %>%
  mutate(month_name = factor(month.name[month], levels = month.name))

# Calculate monthly mean and deviantion from monthly mean across years
# Step 1: Calculate mean rfh for each month across years
rainfall_monthly_mean <- rainfall_month %>%
  group_by(month) %>%
  summarise(mean_rfh = mean(rfh, na.rm = TRUE))

# Step 2: Identify deviation from the mean and direction
rainfall_month <- rainfall_month %>%
  left_join(rainfall_monthly_mean, by = "month") %>%
  mutate(
    deviation = rfh - mean_rfh,  # Deviation size
    above_below_avg = ifelse(deviation > 0, "Above", "Below")
  )

# Plot facets by month
rainfall_plot <- ggplot(rainfall_month %>% filter(year >= 2003), 
                        aes(x = year, y = deviation)) +
  geom_line(color = "steelblue", alpha = 0.6) +  # Original data
  geom_smooth(method = "lm", se = TRUE, color = "black", linewidth = 1) +  # LOESS trend line
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # Reference line at zero
  facet_wrap(~month_name, ncol = 4) +  # 12 facets (3 rows x 4 columns)
  labs(
    title = "Deviation from Monthly Mean RFH by Year",
    x = "",
    y = "Deviation"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = -60, hjust = 0))
rainfall_plot

# Load homicide data
homicide <- read.csv(here("data/HOMICIDIO_20250312.csv"))

# Find month and year in date
homicide <- homicide %>%
  mutate(FECHA.HECHO = dmy(FECHA.HECHO),
         year = year(FECHA.HECHO),
         month = month(FECHA.HECHO))

# Aggregate data monthly
homicide_month <- homicide %>%
  group_by(year, month) %>%
  summarise(homicide = sum(CANTIDAD, na.rm = TRUE)) %>%
  mutate(month_name = factor(month.name[month], levels = month.name))

# Standardize homicide counts within each year
homicide_month <- homicide_month %>%
  group_by(year) %>%
  mutate(standardized_homicide = scale(homicide)[, 1]) %>%
  ungroup()

# Calculate monthly mean (after standardization)
homicide_monthly_mean <- homicide_month %>%
  group_by(month) %>%
  summarise(mean_standardized_homicide = mean(standardized_homicide, na.rm = TRUE))

# Calculate deviation from mean for each month
homicide_month <- homicide_month %>%
  left_join(homicide_monthly_mean, by = "month") %>%
  mutate(deviation = standardized_homicide - mean_standardized_homicide)

# Plot Deviations by Month
homicide_plot <- ggplot(homicide_month, aes(x = year, y = deviation)) +
  geom_line(color = "steelblue", alpha = 0.6) +  # Original data
  geom_smooth(method = "lm", se = TRUE, color = "black", linewidth = 1) +  # LOESS trend line
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # Reference line at zero
  facet_wrap(~month_name, ncol = 4) +  # 12 facets (3 rows x 4 columns)
  labs(
    title = "Deviation from Monthly Mean Homicide by Year",
    x = "",
    y = "Deviation"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = -60, hjust = 0))
homicide_plot

# Plot two graphs together
ggarrange(rainfall_plot, homicide_plot)
ggsave(here('output/deviations_by_year.jpg'), width = 10, height = 5)

# Merge rainfall and homicide data
merged_data <- rainfall_month %>%
  inner_join(homicide_month, by = c("year", "month")) %>%
  select(year, month, deviation.x, deviation.y) %>%
  rename(rainfall_deviation = deviation.x,
         homicide_deviation = deviation.y)

# Rescale all variables of interest
merged_data <- merged_data %>%
  mutate(scaled_rainfall_deviation = scale(rainfall_deviation)[, 1],
         scaled_homicide_deviation = scale(homicide_deviation)[, 1])

# Fixed Effects Model
fe_model <- plm(scaled_homicide_deviation ~ scaled_rainfall_deviation, 
                data = merged_data %>% filter(year != 2025), 
                index = c("year", "month"), 
                model = "within")

summary(fe_model)

# GLS Model with Autocorrelation
gls_model <- gls(scaled_homicide_deviation ~ scaled_rainfall_deviation, 
                 correlation = corAR1(form = ~ year | month), 
                 data = merged_data %>% filter(year != 2025))
summary(gls_model)

# Rename municipality and department codes
rainfall <- rainfall %>%
  mutate(COD_DEPTO = substr(gsub("^CO", "", ADM2_PCODE), 1, 2) %>%
           gsub("^0", "", .),
         COD_DEPTO = as.integer(COD_DEPTO),
         COD_MUNI = gsub("^0", "", gsub("^CO", "", ADM2_PCODE)),
         COD_MUNI = as.integer(COD_MUNI))

# Aggregate rainfall data by municipality and monthly
rainfall_muni_month <- rainfall %>%
  group_by(month, year, COD_MUNI) %>%
  summarise(rfh = sum(rfh)) %>%
  mutate(month_name = factor(month.name[month], levels = month.name))

# Calculate monthly mean and deviantion from municipality and monthly mean across years
# Step 1: Calculate mean rfh for each month across years
rainfall_muni_monthly_mean <- rainfall_muni_month %>%
  group_by(month, COD_MUNI) %>%
  summarise(mean_rfh = mean(rfh, na.rm = TRUE))

# Step 2: Identify deviation from the mean and direction
rainfall_muni_month <- rainfall_muni_month %>%
  left_join(rainfall_muni_monthly_mean, by = c("month", "COD_MUNI")) %>%
  mutate(
    deviation = rfh - mean_rfh,  # Deviation size
    above_below_avg = ifelse(deviation > 0, "Above", "Below")
  )

# Aggregate homicide data by municipality and monthly
homicide_muni_month <- homicide %>%
  group_by(year, month, COD_MUNI) %>%
  summarise(homicide = sum(CANTIDAD, na.rm = TRUE))

# List of months, years and municipalities from rainfall data
muni_month <- rainfall_muni_month %>%
  dplyr::select(month, year, COD_MUNI) %>%
  filter(year >= 2003)

# Left join homicide aggregates to list of months, years and municipalities
# Assume 0 (NA) if is no homicide recorded
homicide_muni_month <- muni_month %>%
  left_join(homicide_muni_month, by = c("month", "year", "COD_MUNI")) %>%
  mutate(homicide = ifelse(is.na(homicide), 0, homicide),
         month_name = factor(month.name[month], levels = month.name))

# Standardize homicide counts within each year
homicide_muni_month <- homicide_muni_month %>%
  group_by(year) %>%
  mutate(standardized_homicide = scale(homicide)[, 1]) %>%
  ungroup()

# Calculate municipality and monthly homicide mean (after standardization)
homicide_muni_monthly_mean <- homicide_muni_month %>%
  group_by(month, COD_MUNI) %>%
  summarise(mean_standardized_homicide = mean(standardized_homicide, na.rm = TRUE))

# Calculate homicide deviation from mean for each municipality and month
homicide_muni_month <- homicide_muni_month %>%
  left_join(homicide_muni_monthly_mean, by = c("month", "COD_MUNI")) %>%
  mutate(deviation = standardized_homicide - mean_standardized_homicide)

# Merge rainfall and homicide municipality data
merged_muni_data <- rainfall_muni_month %>%
  inner_join(homicide_muni_month, by = c("year", "month", "COD_MUNI")) %>%
  select(year, month, COD_MUNI, rfh, mean_rfh, deviation.x, 
         standardized_homicide, mean_standardized_homicide, deviation.y) %>%
  rename(rainfall_deviation = deviation.x,
         homicide_deviation = deviation.y)

# Create new 'time' variable
merged_muni_data <- merged_muni_data %>%
  ungroup() %>%
  mutate(date = as.Date(paste(year, month, "01", sep = "-"))) %>%
  arrange(date) %>%
  mutate(time = dense_rank(date))

# Rescale all variables of interest
merged_muni_data <- merged_muni_data %>%
  mutate(scaled_rainfall_deviation = scale(rainfall_deviation)[, 1],
         scaled_homicide_deviation = scale(homicide_deviation)[, 1])

# Fixed Effects Model without interaction term
fe_dept_model <- plm(scaled_homicide_deviation ~ scaled_rainfall_deviation + factor(COD_MUNI), 
                     data = merged_muni_data %>% filter(year != 2025), 
                     index = c("year", "month"), 
                     model = "within")

summary(fe_dept_model)
##coefficients show how much municipalities contribute towards change in homicide

# Fixed Effects Model with interaction term (so each municipality has its own slope)
fe_int_dept_model <- plm(scaled_homicide_deviation ~ scaled_rainfall_deviation * factor(COD_MUNI), 
                     data = merged_muni_data %>% filter(year != 2025), 
                     index = c("year", "month"), 
                     model = "within")

#saveRDS(fe_int_dept_model, here("output/fe_int_dept_model.rds"))
#fe_int_dept_model <- readRDS(here("output/fe_int_dept_model.rds"))
summary(fe_int_dept_model)

# Extract coefficients table
coeff_fe_int_table <- as.data.frame(coef(summary(fe_int_dept_model)))

# Filter for COD_MUNI interaction terms only
coeff_fe_int_table <- coeff_fe_int_table[grep("scaled_rainfall_deviation:factor\\(COD_MUNI\\)", rownames(coeff_fe_int_table)), ]

# COD_MUNI identifiers
coeff_fe_int_table$COD_MUNI <- gsub("scaled_rainfall_deviation:factor\\(COD_MUNI\\)", "", rownames(coeff_fe_int_table))
coeff_fe_int_table <- coeff_fe_int_table %>%
  mutate(COD_MUNI = as.integer(COD_MUNI))





library(sf)
library(spdep)

# Load shapefile data
shapefile_data <- st_read(here("data/Municipios_USAID/Municipios_USAID.shp"))

# Check CRS
#st_crs(shapefile_data)

# Select only variables of interest and extract centroids from polygons
shapefile_data <- shapefile_data %>%
  mutate(COD_MUNI = as.integer(MPIO_CCDGO)) %>%
  dplyr::select(COD_MUNI, MPIO_CNMBR) %>%
  mutate(LONG = st_coordinates(st_centroid(geometry))[, 1],
         LAT = st_coordinates(st_centroid(geometry))[, 2])

# Calculate mean rainfall and homicide deviatios for last 3 years (random number of years for visualization)
merged_muni_data_3y <- merged_muni_data %>%
  filter(year >= 2020 & year < 2025) %>%
  group_by(COD_MUNI, month) %>%
  summarise(
    mean_rainfall_deviation = mean(rainfall_deviation, na.rm = TRUE),
    mean_rainfall = mean(rfh, na.rm = TRUE),
    monthly_mean_rainfall_all = mean(mean_rfh, na.rm = TRUE),
    mean_homicide_deviation = mean(homicide_deviation, na.rm = TRUE),
    mean_homicide = mean(standardized_homicide, na.rm = TRUE),
    monthly_mean_homicide_all = mean(mean_standardized_homicide, na.rm = TRUE)
  ) %>%
  mutate(
    mean_rainfall_relative_dif = (mean_rainfall - monthly_mean_rainfall_all) / monthly_mean_rainfall_all * 100,
    mean_homicide_relative_dif = (mean_homicide - monthly_mean_homicide_all) / monthly_mean_homicide_all * 100
  )

# Removing datasets from environment
rm(fe_int_dept_model, homicide, homicide_month, homicide_monthly_mean,
   homicide_muni_month, homicide_muni_monthly_mean, merged_data, muni_month,
   rainfall, rainfall_month, rainfall_muni_month, rainfall_muni_monthly_mean,
   rainfall_plot, rainfall_monthly_mean)
   
# Merge shapefile with 3-year mean rainfall and homicide data
merged_muni_data_3y <- merged_muni_data_3y %>%
  full_join(shapefile_data, by = "COD_MUNI")

# Also link coefficients from fixed effect model
merged_muni_data_3y <- merged_muni_data_3y %>%
  left_join(coeff_fe_int_table, by = "COD_MUNI") %>%
  mutate(month_name = factor(month.name[month], levels = month.name))

# Reassign the sf class after join
merged_muni_data_3y <- st_as_sf(merged_muni_data_3y, crs = st_crs(shapefile_data))

# New data to plot regression estimates
merged_muni_data_slopes <- merged_muni_data_3y %>%
  dplyr::select(COD_MUNI, MPIO_CNMBR, geometry, Estimate, `Std. Error`,
                `Pr(>|t|)`) %>%
  unique()

# Plot regression estimates
estimates_map <- ggplot(data = merged_muni_data_slopes) +
  geom_sf(aes(fill = Estimate), colour = NA) +
  scale_fill_viridis_c(option = "viridis", na.value = "grey80",
                       guide = guide_colourbar(title = NULL)) +
  labs(
    title = "Municipality-Specific Rainfall Effects",
    fill = ""
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    strip.text = element_text(size = 10, face = "bold"),
    legend.position = "bottom"
  )
estimates_map

ggsave(here('output/estimates_map.jpg'), width = 7, height = 7)

# Simplify the geometries (for computational efficiency)
merged_muni_data_3y <- merged_muni_data_3y %>%
  st_simplify(dTolerance = 0.01) 

# Create spatial weights matrix (Queen contiguity)
#nb <- poly2nb(merged_muni_data_3y %>% filter(month == 1)) 
nb <- poly2nb(merged_muni_data_3y) 
lw <- nb2listw(nb, style = "W", zero.policy = TRUE)

# Compute LISA for rainfall deviation
lisa_rainfall <- localmoran(merged_muni_data_3y$mean_rainfall_deviation, lw, zero.policy = TRUE)

# Add LISA values to dataset
merged_muni_data_3y <- merged_muni_data_3y %>%
  mutate(
    moran_I_rain = lisa_rainfall[, 1],
    p_value_rain = lisa_rainfall[, 5]
  )

# Compute LISA for rainfall deviation each month
merged_muni_data_3y <- merged_muni_data_3y %>%
  filter(!is.na(month)) %>%
  mutate(
    cluster_rain = case_when(
      moran_I_rain > 0 & p_value_rain <= 0.05 ~ "High-High (Hotspot)",
      moran_I_rain < 0 & p_value_rain <= 0.05 ~ "Low-Low (Coldspot)",
      moran_I_rain > 0 & p_value_rain > 0.05 ~ "Non-significant High",
      moran_I_rain < 0 & p_value_rain > 0.05 ~ "Non-significant Low",
      TRUE ~ "Non-significant"
    )
  )

# LISA for Homicide
lisa_homicide <- localmoran(merged_muni_data_3y$mean_homicide_deviation, lw, zero.policy = TRUE)

merged_muni_data_3y <- merged_muni_data_3y %>%
  mutate(
    moran_I_homicide = lisa_homicide[, 1],
    p_value_homicide = lisa_homicide[, 5]
  )

# Assign clusters using global thresholds
merged_muni_data_3y <- merged_muni_data_3y %>%
  filter(!is.na(month)) %>%
  mutate(
    cluster_homicide = case_when(
      moran_I_homicide > 0 & p_value_homicide <= 0.05 ~ "High-High (Hotspot)",
      moran_I_homicide < 0 & p_value_homicide <= 0.05 ~ "Low-Low (Coldspot)",
      moran_I_homicide > 0 & p_value_homicide > 0.05 ~ "Non-significant High",
      moran_I_homicide < 0 & p_value_homicide > 0.05 ~ "Non-significant Low",
      TRUE ~ "Non-significant"
    )
  )

# Compute LISA for rainfall deviation each month
#lisa_results_rainfall <- merged_muni_data_3y %>%
#  filter(!is.na(month)) %>%
#  group_by(month) %>%
#  mutate(
#    local_moran_I_rain = localmoran(mean_rainfall_deviation, lw, zero.policy = TRUE)[, 1],
#    p_value_rain = localmoran(mean_rainfall_deviation, lw, zero.policy = TRUE)[, 5],
#    cluster_rain = case_when(
#      local_moran_I_rain > 0 & p_value_rain <= 0.05 ~ "High-High (Hotspot)",
#      local_moran_I_rain < 0 & p_value_rain <= 0.05 ~ "Low-Low (Coldspot)",
#      local_moran_I_rain > 0 & p_value_rain > 0.05 ~ "Non-significant High",
#      local_moran_I_rain < 0 & p_value_rain > 0.05 ~ "Non-significant Low",
#      TRUE ~ "Non-significant"
#    )
#  ) %>%
#  ungroup() %>%
#  dplyr::select(COD_MUNI, month, local_moran_I_rain, p_value_rain, cluster_rain) %>%
#  st_drop_geometry()

# Compute LISA for homicide deviation each month
#lisa_results_homicide <- merged_muni_data_3y %>%
#  filter(!is.na(month)) %>%
#  group_by(month) %>%
#  mutate(
#    local_moran_I_homicide = localmoran(mean_homicide_deviation, lw, zero.policy = TRUE)[, 1],
#    p_value_homicide = localmoran(mean_homicide_deviation, lw, zero.policy = TRUE)[, 5],
#    cluster_homicide = case_when(
#      local_moran_I_homicide > 0 & p_value_homicide <= 0.05 ~ "High-High (Hotspot)",
#      local_moran_I_homicide < 0 & p_value_homicide <= 0.05 ~ "Low-Low (Coldspot)",
#      local_moran_I_homicide > 0 & p_value_homicide > 0.05 ~ "Non-significant High",
#      local_moran_I_homicide < 0 & p_value_homicide > 0.05 ~ "Non-significant Low",
#      TRUE ~ "Non-significant"
#    )
#  ) %>%
#  ungroup() %>%
#  dplyr::select(COD_MUNI, month, local_moran_I_homicide, p_value_homicide, cluster_homicide) %>%
#  st_drop_geometry()

# Left join LISA results
#merged_muni_data_3y <- merged_muni_data_3y %>%
#  filter(!is.na(month)) %>%
#  left_join(lisa_results_rainfall, by = c("COD_MUNI", "month")) %>%
#  left_join(lisa_results_homicide, by = c("COD_MUNI", "month"))

# Plot 3 year average homicide deviation by municipality
homicide_map <- ggplot(merged_muni_data_3y) +
  geom_sf(aes(fill = cluster_homicide), colour = NA) +
  scale_fill_manual(
    values = c(
      "High-High (Hotspot)" = "red",
      "Low-Low (Coldspot)" = "blue",
      "Non-significant High" = "pink",
      "Non-significant Low" = "lightblue",
      "Non-significant" = "grey80"
    )
  ) +  
  facet_wrap(~month, ncol = 4) +
  labs(
    title = "LISA Homicide Deviation Clusters by Month",
    fill = "Cluster Type"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    strip.text = element_text(size = 10, face = "bold"),
    legend.position = "bottom"
  )

# Plot 3 year average rainfall deviation by municipality
rainfall_map <- ggplot(merged_muni_data_3y) +
  geom_sf(aes(fill = cluster_rain), colour = NA) +
  scale_fill_manual(
    values = c(
      "High-High (Hotspot)" = "red",
      "Low-Low (Coldspot)" = "blue",
      "Non-significant High" = "pink",
      "Non-significant Low" = "lightblue",
      "Non-significant" = "grey80"
    )
  ) +  
  facet_wrap(~month, ncol = 4) +
  labs(
    title = "LISA Rainfall Deviation Clusters by Month",
    fill = "Cluster Type"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    strip.text = element_text(size = 10, face = "bold"),
    legend.position = "bottom"
  )

# Plot two maps together
ggarrange(rainfall_map, homicide_map, common.legend = TRUE, legend="bottom")
ggsave(here('output/rainfall_homicide_LISA_maps.jpg'), width = 10, height = 7)








 








# Plot 3 year average rainfall deviation by municipality
rainfall_map <- ggplot(data = merged_muni_data_3y %>% filter(!is.na(month))) +
  geom_sf(aes(fill = mean_rainfall_deviation), colour = NA) +
  scale_fill_gradient2(
    low = "darkblue",      # Color for negative values
    mid = "lightgrey",     # Neutral (zero) point
    high = "darkred",      # Color for positive values
    midpoint = 0,      # Zero is the neutral point
    na.value = "white",
    guide = guide_colourbar(title = NULL)
  ) +
  facet_wrap(~month_name, ncol = 4) +
  labs(
    title = "Mean Rainfall Deviation (2023-2024)",
    fill = ""
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    strip.text = element_text(size = 10, face = "bold"),
    legend.position = "bottom"
  )

# Plot 3 year average homicide deviation by municipality
homicide_map <- ggplot(data = merged_muni_data_3y %>% filter(!is.na(month))) +
  geom_sf(aes(fill = mean_homicide_deviation), colour = NA) +
  scale_fill_gradient2(
    low = "darkblue",    # Colour for negative values
    mid = "lightgrey",     # Neutral (zero) point
    high = "darkred",   # Colour for positive values
    midpoint = 0,      # Ensures zero is the neutral point
    na.value = "white",
    guide = guide_colourbar(title = NULL)
  ) +
  facet_wrap(~month_name, ncol = 4) +
  labs(
    title = "Mean Homicide Deviation (2023-2024)",
    fill = ""
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    strip.text = element_text(size = 10, face = "bold"),
    legend.position = "bottom"
  )

# Plot two maps together
ggarrange(rainfall_map, homicide_map)
ggsave(here('output/rainfall_homicide_maps.jpg'), width = 10, height = 7)












rm(list = ls())
options(scipen=999)

# Load packages
library(here)
library(ggplot2)
library(plm)
library(nlme)
library(sf)
library(spdep)
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

## checking
fe_model_2 <- lm(scaled_homicide_deviation ~ scaled_rainfall_deviation + factor(year) + factor(month), 
                data = merged_data %>% filter(year != 2025))

summary(fe_model_2)

## beta = -0.176146 | exactly the same model, as expected. all good.

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

# Aggregate rainfall data by department and monthly
rainfall_dep_month <- rainfall %>%
  group_by(month, year, COD_DEPTO) %>%
  summarise(rfh = sum(rfh)) %>%
  mutate(month_name = factor(month.name[month], levels = month.name))

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

# Aggregate homicide data by department and monthly
homicide_dep_month <- homicide %>%
  group_by(month, year, COD_DEPTO) %>%
  summarise(homicide = sum(CANTIDAD, na.rm = TRUE)) %>%
  mutate(month_name = factor(month.name[month], levels = month.name))

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

### TO: double checking
fe_dept_model_2 <- lm(scaled_homicide_deviation ~ scaled_rainfall_deviation + factor(COD_MUNI) + factor(year) + factor(month), 
                     data = merged_muni_data %>% filter(year != 2025))

library(texreg)
screenreg(fe_dept_model_2, digits = 7, omit.coef = "factor\\(.*\\)|gender|race|education")

# beta = -0.0031341 | p = 0.098
# beta = -0.0029468 | p = 0.122

## don't know why they're not exactly the same, but any differences are negligent anyway

fe_dept_model_3 <- plm(scaled_homicide_deviation ~ scaled_rainfall_deviation, 
                     data = merged_muni_data %>% filter(year != 2025), 
                     index = c("year", "month", "COD_MUNI"), 
                     model = "within")
summary(fe_dept_model_3)

# beta = -0.0031048 | p = 0.099

# Fixed Effects Model with interaction term (so each municipality has its own slope)
fe_int_dept_model <- plm(scaled_homicide_deviation ~ scaled_rainfall_deviation * factor(COD_MUNI), 
                     data = merged_muni_data %>% filter(year != 2025), 
                     index = c("year", "month"), 
                     model = "within")

library(lme4)
lme_int_dept_model <- lmer(scaled_homicide_deviation ~ scaled_rainfall_deviation + 
                             (scaled_rainfall_deviation | COD_MUNI) + factor(month) + factor(year),
                           data = merged_muni_data %>% filter(year != 2025))

summary(lme_int_dept_model)

# consistent results again!

#saveRDS(fe_int_dept_model, here("output/fe_int_dept_model.rds"))
#fe_int_dept_model <- readRDS(here("output/fe_int_dept_model.rds"))
#summary(fe_int_dept_model)

# Extract coefficients table
#coeff_fe_int_table <- as.data.frame(coef(summary(fe_int_dept_model)))

# Filter for COD_MUNI interaction terms only
#coeff_fe_int_table <- coeff_fe_int_table[grep("scaled_rainfall_deviation:factor\\(COD_MUNI\\)", rownames(coeff_fe_int_table)), ]

# COD_MUNI identifiers
#coeff_fe_int_table$COD_MUNI <- gsub("scaled_rainfall_deviation:factor\\(COD_MUNI\\)", "", rownames(coeff_fe_int_table))
#coeff_fe_int_table <- coeff_fe_int_table %>%
#  mutate(COD_MUNI = as.integer(COD_MUNI))

#write.csv(coeff_fe_int_table, here("output/coeff_fe_int_table.csv"))
coeff_fe_int_table <- read.csv(here("output/coeff_fe_int_table.csv"))

# Load shapefile data
shapefile_data <- st_read(here("data/Municipios_USAID/Municipios_USAID.shp"))

# Check CRS
#st_crs(shapefile_data)

# Aggregate polygons by DPTO_CCDGO
#department_polygons <- shapefile_data %>%
#  group_by(DPTO_CCDGO) %>%
#  summarise(geometry = st_union(geometry)) %>%
#  ungroup()

# Optional: Save the new shapefile
#st_write(department_polygons, here("data/departamentos/Departamento_Polygons.shp"))

# Load departments shapefile data
department_polygons <- st_read(here("data/departamentos/Departamento_Polygons.shp"))

# Calculate monthly mean and deviantion from department and monthly mean across years
# Step 1: Calculate mean rfh for each month across years
rainfall_dep_monthly_mean <- rainfall_dep_month %>%
  group_by(month, COD_DEPTO) %>%
  summarise(mean_rfh = mean(rfh, na.rm = TRUE))

# Step 2: Identify deviation from the mean and direction
rainfall_dep_month <- rainfall_dep_month %>%
  left_join(rainfall_dep_monthly_mean, by = c("month", "COD_DEPTO")) %>%
  mutate(
    deviation = rfh - mean_rfh,  # Deviation size
    above_below_avg = ifelse(deviation > 0, "Above", "Below")
  )

# List of months, years and departments from rainfall data
dep_month <- rainfall_dep_month %>%
  dplyr::select(month, year, COD_DEPTO) %>%
  filter(year >= 2003)

# Left join homicide aggregates to list of months, years and departments
# Assume 0 (NA) if is no homicide recorded
homicide_dep_month <- dep_month %>%
  left_join(homicide_dep_month, by = c("month", "year", "COD_DEPTO")) %>%
  mutate(homicide = ifelse(is.na(homicide), 0, homicide),
         month_name = factor(month.name[month], levels = month.name))

# Standardize homicide counts within each year
homicide_dep_month <- homicide_dep_month %>%
  group_by(year) %>%
  mutate(standardized_homicide = scale(homicide)[, 1]) %>%
  ungroup()

# Calculate department and monthly homicide mean (after standardization)
homicide_dep_monthly_mean <- homicide_dep_month %>%
  group_by(month, COD_DEPTO) %>%
  summarise(mean_standardized_homicide = mean(standardized_homicide, na.rm = TRUE))

# Calculate homicide deviation from mean for each department and month
homicide_dep_month <- homicide_dep_month %>%
  left_join(homicide_dep_monthly_mean, by = c("month", "COD_DEPTO")) %>%
  mutate(deviation = standardized_homicide - mean_standardized_homicide)

# Merge rainfall and homicide department data
merged_dep_data <- rainfall_dep_month %>%
  inner_join(homicide_dep_month, by = c("year", "month", "COD_DEPTO")) %>%
  select(year, month, COD_DEPTO, rfh, mean_rfh, deviation.x, 
         standardized_homicide, mean_standardized_homicide, deviation.y) %>%
  rename(rainfall_deviation = deviation.x,
         homicide_deviation = deviation.y)

# Calculate mean rainfall and homicide deviations for last 3 years (random number of years for visualization)
merged_dep_data_3y <- merged_dep_data %>%
  filter(year >= 2022 & year < 2025) %>%
  group_by(COD_DEPTO, month) %>%
  summarise(
    mean_rainfall_deviation = mean(rainfall_deviation, na.rm = TRUE),
    mean_rainfall = mean(rfh, na.rm = TRUE),
    monthly_mean_rainfall_all = mean(mean_rfh, na.rm = TRUE),
    mean_homicide_deviation = mean(homicide_deviation, na.rm = TRUE),
    mean_homicide = mean(standardized_homicide, na.rm = TRUE),
    monthly_mean_homicide_all = mean(mean_standardized_homicide, na.rm = TRUE)
  )

# Convert DPTO_CCDGO to integer, removing leading zeros
department_polygons$COD_DEPTO <- as.integer(department_polygons$DPTO_CCDGO)

# Merge shapefile with 3-year mean rainfall and homicide data
merged_dep_data_3y <- merged_dep_data_3y %>%
  full_join(department_polygons, by = "COD_DEPTO") %>%
  mutate(month_name = factor(month.name[month], levels = month.name))

# Reassign the sf class after join
merged_dep_data_3y <- st_as_sf(merged_dep_data_3y, crs = st_crs(department_polygons))

# Plot 3 year average homicide deviation by department
homicide_map_dep <- ggplot(data = merged_dep_data_3y) +
  geom_sf(aes(fill = mean_homicide_deviation), colour = NA) +
  scale_fill_gradient2(
    low = "darkblue",    # Colour for negative values
    mid = "lightgrey",     # Neutral (zero) point
    high = "darkred",   # Colour for positive values
    midpoint = 0,      # Ensures zero is the neutral point
    #limits = c(-2, 2),
    na.value = "white",
    guide = guide_colourbar(title = NULL),
    labels = scales::number_format(accuracy = 0.1)
  ) +
  facet_wrap(~month_name, ncol = 4) +
  labs(
    title = "Mean Homicide Deviation (2022-2024)",
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

# At municipal level, select only variables of interest and extract centroids from polygons
shapefile_data <- shapefile_data %>%
  mutate(COD_MUNI = as.integer(MPIO_CCDGO)) %>%
  dplyr::select(COD_MUNI, MPIO_CNMBR) %>%
  mutate(LONG = st_coordinates(st_centroid(geometry))[, 1],
         LAT = st_coordinates(st_centroid(geometry))[, 2])

# Calculate mean rainfall and homicide deviatios for last 3 years (random number of years for visualization)
merged_muni_data_3y <- merged_muni_data %>%
  filter(year >= 2022 & year < 2025) %>%
  group_by(COD_MUNI, month) %>%
  summarise(
    mean_rainfall_deviation = mean(rainfall_deviation, na.rm = TRUE),
    mean_rainfall = mean(rfh, na.rm = TRUE),
    monthly_mean_rainfall_all = mean(mean_rfh, na.rm = TRUE),
    mean_homicide_deviation = mean(homicide_deviation, na.rm = TRUE),
    mean_homicide = mean(standardized_homicide, na.rm = TRUE),
    monthly_mean_homicide_all = mean(mean_standardized_homicide, na.rm = TRUE)
  )

# Removing datasets from environment
rm(fe_int_dept_model, homicide, homicide_month, homicide_monthly_mean,
   homicide_muni_month, homicide_muni_monthly_mean, merged_data, muni_month,
   rainfall, rainfall_month, rainfall_muni_month, rainfall_muni_monthly_mean,
   rainfall_plot, rainfall_monthly_mean)
   
# Merge shapefile with 5-year mean rainfall and homicide data
merged_muni_data_3y <- merged_muni_data_3y %>%
  full_join(shapefile_data, by = "COD_MUNI")

# Also link coefficients from fixed effect model
merged_muni_data_3y <- merged_muni_data_3y %>%
  left_join(coeff_fe_int_table, by = "COD_MUNI") %>%
  mutate(month_name = factor(month.name[month], levels = month.name)) %>%
  dplyr::select(-X) %>%
  filter(!is.na(month))

# Reassign the sf class after join
merged_muni_data_3y <- st_as_sf(merged_muni_data_3y, crs = st_crs(shapefile_data))

# New data to plot regression estimates
merged_muni_data_slopes <- merged_muni_data_3y %>%
  dplyr::select(COD_MUNI, MPIO_CNMBR, geometry, Estimate, Std..Error,
                Pr...t..) %>%
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

# Plot 3 year average rainfall deviation by municipality
rainfall_map <- ggplot(data = merged_muni_data_3y) +
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
    title = "Mean RFH Deviation (2022-2024)",
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
ggarrange(rainfall_map, homicide_map_dep)
ggsave(here('output/rainfall_homicide_maps_final.jpg'), width = 11, height = 7)

# Plot three maps together
ggarrange(
  rainfall_map, homicide_map_dep,
  estimates_map,
  ncol = 2, nrow = 2,
  widths = c(1, 1),
  heights = c(1, 0.6)
)
ggsave(here('output/maps_final.jpg'), width = 10, height = 12)







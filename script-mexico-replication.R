
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
library(tidyr)
library(dplyr)

# Load precipitation data
rainfall1 <- read.csv(here("data/mexico-replication/mex-rainfall-subnat-part1.csv"))
rainfall2 <- read.csv(here("data/mexico-replication/mex-rainfall-subnat-part2.csv"))
rainfall3 <- read.csv(here("data/mexico-replication/mex-rainfall-subnat-part3.csv"))
rainfall4 <- read.csv(here("data/mexico-replication/mex-rainfall-subnat-part4.csv"))
rainfall <- rainfall1 %>%
  rbind(rainfall2, rainfall3, rainfall4)

rm(rainfall1, rainfall2, rainfall3, rainfall4)

# Find month and year in date
rainfall <- rainfall %>%
  mutate(year = year(X.date),
         month = month(X.date)) %>%
  filter(year >= 1997) %>%
  filter(X.adm.level.level == "1")

# Aggregate data monthly
rainfall_month <- rainfall %>%
  group_by(month, year) %>%
  summarise(rfh = sum(X.indicator.rfh.num)) %>%
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
rainfall_plot <- ggplot(rainfall_month %>% filter(year >= 1997), 
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
homicide <- read.csv(here("data/mexico-replication/homicide1997-2017.csv"))
homicide2 <- read.csv(here("data/mexico-replication/homicide2015-2025.csv"))

# Select only homicides
homicide <- homicide %>%
  filter(MODALIDAD == "HOMICIDIOS") %>%
  filter(ENTIDAD != "MEXICO")
homicide2 <- homicide2 %>%
  filter(Tipo.de.delito == "Homicidio") %>%
  filter(Entidad != "M\xe9xico")

# Change to long format
homicide <- homicide %>%
  pivot_longer(
    cols = ENERO:DICIEMBRE,
    names_to = "month_name",
    values_to = "value"
  ) %>%
  mutate(
    month = case_when(
      month_name == "ENERO" ~ 1,
      month_name == "FEBRERO" ~ 2,
      month_name == "MARZO" ~ 3,
      month_name == "ABRIL" ~ 4,
      month_name == "MAYO" ~ 5,
      month_name == "JUNIO" ~ 6,
      month_name == "JULIO" ~ 7,
      month_name == "AGOSTO" ~ 8,
      month_name == "SEPTIEMBRE" ~ 9,
      month_name == "OCTUBRE" ~ 10,
      month_name == "NOVIEMBRE" ~ 11,
      month_name == "DICIEMBRE" ~ 12
    )
  ) %>%
  group_by(ANO, month) %>%
  summarise(total = sum(value, na.rm = TRUE), .groups = "drop") %>%
  rename(year = ANO) %>%
  filter(year < 2015)

homicide2 <- homicide2 %>%
  pivot_longer(
    cols = Enero:Diciembre,
    names_to = "month_name",
    values_to = "value"
  ) %>%
  mutate(
    month = case_when(
      month_name == "Enero" ~ 1,
      month_name == "Febrero" ~ 2,
      month_name == "Marzo" ~ 3,
      month_name == "Abril" ~ 4,
      month_name == "Mayo" ~ 5,
      month_name == "Junio" ~ 6,
      month_name == "Julio" ~ 7,
      month_name == "Agosto" ~ 8,
      month_name == "Septiembre" ~ 9,
      month_name == "Octubre" ~ 10,
      month_name == "Noviembre" ~ 11,
      month_name == "Diciembre" ~ 12
    )
  ) %>%
  group_by(Ano, month) %>%
  summarise(total = sum(value, na.rm = TRUE), .groups = "drop") %>%
  rename(year = Ano)

homicide <- homicide %>%
  rbind(homicide2)

# Standardize homicide counts within each year
homicide_month <- homicide %>%
  group_by(year) %>%
  mutate(standardized_homicide = scale(total)[, 1]) %>%
  ungroup()

# Calculate monthly mean (after standardization)
homicide_monthly_mean <- homicide_month %>%
  group_by(month) %>%
  summarise(mean_standardized_homicide = mean(standardized_homicide, na.rm = TRUE))

# Calculate deviation from mean for each month
homicide_month <- homicide_month %>%
  left_join(homicide_monthly_mean, by = "month") %>%
  mutate(deviation = standardized_homicide - mean_standardized_homicide)

# Add month name
homicide_month <- homicide_month %>%
  mutate(month_name = month.name[month])

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
#ggsave(here('output/deviations_by_year.jpg'), width = 10, height = 5)

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

# Rename state codes
state_lookup <- tibble::tribble(
  ~X.adm.code, ~state,
  "MX01", "AGUASCALIENTES",
  "MX02", "BAJA CALIFORNIA",
  "MX03", "BAJA CALIFORNIA SUR",
  "MX04", "CAMPECHE",
  "MX05", "COAHUILA DE ZARAGOZA",
  "MX06", "COLIMA",
  "MX07", "CHIAPAS",
  "MX08", "CHIHUAHUA",
  "MX09", "CIUDAD DE MEXICO",
  "MX10", "DURANGO",
  "MX11", "GUANAJUATO",
  "MX12", "GUERRERO",
  "MX13", "HIDALGO",
  "MX14", "JALISCO",
  "MX15", "MEXICO",
  "MX16", "MICHOACAN",
  "MX17", "MORELOS",
  "MX18", "NAYARIT",
  "MX19", "NUEVO LEON",
  "MX20", "OAXACA",
  "MX21", "PUEBLA",
  "MX22", "QUERETARO",
  "MX23", "QUINTANA ROO",
  "MX24", "SAN LUIS POTOSI",
  "MX25", "SINALOA",
  "MX26", "SONORA",
  "MX27", "TABASCO",
  "MX28", "TAMAULIPAS",
  "MX29", "TLAXCALA",
  "MX30", "VERACRUZ",
  "MX31", "YUCATAN",
  "MX32", "ZACATECAS"
)

rainfall <- rainfall %>%
  left_join(state_lookup, by = "X.adm.code")

# Aggregate rainfall data by state and monthly
rainfall_state_month <- rainfall %>%
  group_by(month, year, state) %>%
  summarise(rfh = sum(X.indicator.rfh.num)) %>%
  mutate(month_name = factor(month.name[month], levels = month.name))

# Calculate monthly mean and deviantion from state and monthly mean across years
# Step 1: Calculate mean rfh for each month across years
rainfall_state_monthly_mean <- rainfall_state_month %>%
  group_by(month, state) %>%
  summarise(mean_rfh = mean(rfh, na.rm = TRUE))

# Step 2: Identify deviation from the mean and direction
rainfall_state_month <- rainfall_state_month %>%
  left_join(rainfall_state_monthly_mean, by = c("month", "state")) %>%
  mutate(
    deviation = rfh - mean_rfh,  # Deviation size
    above_below_avg = ifelse(deviation > 0, "Above", "Below")
  )

# Load homicide data
homicide <- read.csv(here("data/mexico-replication/homicide1997-2017.csv"))
homicide2 <- read.csv(here("data/mexico-replication/homicide2015-2025.csv"))

# Select only homicides
homicide <- homicide %>%
  filter(MODALIDAD == "HOMICIDIOS")
homicide2 <- homicide2 %>%
  filter(Tipo.de.delito == "Homicidio")

# Change to long format
homicide <- homicide %>%
  pivot_longer(
    cols = ENERO:DICIEMBRE,
    names_to = "month_name",
    values_to = "value"
  ) %>%
  mutate(
    month = case_when(
      month_name == "ENERO" ~ 1,
      month_name == "FEBRERO" ~ 2,
      month_name == "MARZO" ~ 3,
      month_name == "ABRIL" ~ 4,
      month_name == "MAYO" ~ 5,
      month_name == "JUNIO" ~ 6,
      month_name == "JULIO" ~ 7,
      month_name == "AGOSTO" ~ 8,
      month_name == "SEPTIEMBRE" ~ 9,
      month_name == "OCTUBRE" ~ 10,
      month_name == "NOVIEMBRE" ~ 11,
      month_name == "DICIEMBRE" ~ 12
    )
  ) %>%
  group_by(ANO, month, ENTIDAD) %>%
  summarise(total = sum(value, na.rm = TRUE), .groups = "drop") %>%
  rename(year = ANO) %>%
  filter(year < 2015)

homicide2 <- homicide2 %>%
  pivot_longer(
    cols = Enero:Diciembre,
    names_to = "month_name",
    values_to = "value"
  ) %>%
  mutate(
    month = case_when(
      month_name == "Enero" ~ 1,
      month_name == "Febrero" ~ 2,
      month_name == "Marzo" ~ 3,
      month_name == "Abril" ~ 4,
      month_name == "Mayo" ~ 5,
      month_name == "Junio" ~ 6,
      month_name == "Julio" ~ 7,
      month_name == "Agosto" ~ 8,
      month_name == "Septiembre" ~ 9,
      month_name == "Octubre" ~ 10,
      month_name == "Noviembre" ~ 11,
      month_name == "Diciembre" ~ 12
    )
  ) %>%
  group_by(Ano, month, Entidad) %>%
  summarise(total = sum(value, na.rm = TRUE), .groups = "drop") %>%
  rename(year = Ano,
         ENTIDAD = Entidad)

# Rename state names
state_lookup2 <- tibble::tribble(
  ~ENTIDAD, ~state,
  "Aguascalientes", "AGUASCALIENTES",
  "Baja California", "BAJA CALIFORNIA",
  "Baja California Sur", "BAJA CALIFORNIA SUR",
  "Campeche", "CAMPECHE",
  "Coahuila de Zaragoza", "COAHUILA DE ZARAGOZA",
  "Colima", "COLIMA",
  "Chiapas", "CHIAPAS",
  "Chihuahua", "CHIHUAHUA",
  "Ciudad de M\xe9xico", "CIUDAD DE MEXICO",
  "Durango", "DURANGO",
  "Guanajuato", "GUANAJUATO",
  "Guerrero", "GUERRERO",
  "Hidalgo", "HIDALGO",
  "Jalisco", "JALISCO",
  "MX15", "MEXICO",
  "Michoac\xe1n de Ocampo", "MICHOACAN",
  "Morelos", "MORELOS",
  "Nayarit", "NAYARIT",
  "Nuevo Le\xf3n", "NUEVO LEON",
  "Oaxaca", "OAXACA",
  "Puebla", "PUEBLA",
  "Quer\xe9taro", "QUERETARO",
  "Quintana Roo", "QUINTANA ROO",
  "San Luis Potos\xed", "SAN LUIS POTOSI",
  "Sinaloa", "SINALOA",
  "Sonora", "SONORA",
  "Tabasco", "TABASCO",
  "Tamaulipas", "TAMAULIPAS",
  "Tlaxcala", "TLAXCALA",
  "Veracruz de Ignacio de la Llave", "VERACRUZ",
  "Yucat\xe1n", "YUCATAN",
  "Zacatecas", "ZACATECAS"
)

homicide2 <- homicide2 %>%
  left_join(state_lookup2, by = "ENTIDAD") %>%
  dplyr::select(year, month, state, total) %>%
  rename(ENTIDAD = state)

homicide <- homicide %>%
  rbind(homicide2)

# Aggregate homicide data by state and monthly
homicide_state_month <- homicide %>%
  group_by(month, year, ENTIDAD) %>%
  summarise(homicide = sum(total, na.rm = TRUE)) %>%
  mutate(month_name = factor(month.name[month], levels = month.name))

# Aggregate homicide data by state and monthly
homicide_state_month <- homicide %>%
  group_by(year, month, ENTIDAD) %>%
  summarise(homicide = sum(total, na.rm = TRUE))

# List of months, years and states from rainfall data
state_month <- rainfall_state_month %>%
  dplyr::select(month, year, state) %>%
  filter(year >= 2003) %>%
  rename(ENTIDAD = state)

# Left join homicide aggregates to list of months, years and municipalities
# Assume 0 (NA) if is no homicide recorded
homicide_state_month <- state_month %>%
  left_join(homicide_state_month, by = c("month", "year", "ENTIDAD")) %>%
  mutate(homicide = ifelse(is.na(homicide), 0, homicide),
         month_name = factor(month.name[month], levels = month.name))

# Standardize homicide counts within each year
homicide_state_month <- homicide_state_month %>%
  group_by(year) %>%
  mutate(standardized_homicide = scale(homicide)[, 1]) %>%
  ungroup()

# Calculate state and monthly homicide mean (after standardization)
homicide_state_monthly_mean <- homicide_state_month %>%
  group_by(month, ENTIDAD) %>%
  summarise(mean_standardized_homicide = mean(standardized_homicide, na.rm = TRUE))

# Calculate homicide deviation from mean for each state and month
homicide_state_month <- homicide_state_month %>%
  left_join(homicide_state_monthly_mean, by = c("month", "ENTIDAD")) %>%
  mutate(deviation = standardized_homicide - mean_standardized_homicide)

# Merge rainfall and homicide state data
rainfall_state_month <- rainfall_state_month %>%
  rename(ENTIDAD = state)

merged_state_data <- rainfall_state_month %>%
  inner_join(homicide_state_month, by = c("year", "month", "ENTIDAD")) %>%
  select(year, month, ENTIDAD, rfh, mean_rfh, deviation.x, 
         standardized_homicide, mean_standardized_homicide, deviation.y) %>%
  rename(rainfall_deviation = deviation.x,
         homicide_deviation = deviation.y)

# Create new 'time' variable
merged_state_data <- merged_state_data %>%
  ungroup() %>%
  mutate(date = as.Date(paste(year, month, "01", sep = "-"))) %>%
  arrange(date) %>%
  mutate(time = dense_rank(date))

# Rescale all variables of interest
merged_state_data <- merged_state_data %>%
  mutate(scaled_rainfall_deviation = scale(rainfall_deviation)[, 1],
         scaled_homicide_deviation = scale(homicide_deviation)[, 1])

# Fixed Effects Model without interaction term
fe_state_model <- plm(scaled_homicide_deviation ~ scaled_rainfall_deviation + factor(ENTIDAD), 
                     data = merged_state_data %>% filter(year != 2025), 
                     index = c("year", "month"), 
                     model = "within")

summary(fe_state_model)
##coefficients show how much states contribute towards change in homicide

### TO: double checking
fe_state_model_2 <- lm(scaled_homicide_deviation ~ scaled_rainfall_deviation + factor(ENTIDAD) + factor(year) + factor(month), 
                     data = merged_state_data %>% filter(year != 2025))

summary(fe_state_model_2)

library(texreg)
screenreg(fe_state_model_2, digits = 7, omit.coef = "factor\\(.*\\)|gender|race|education")

# beta = -0.0031341 | p = 0.098
# beta = -0.0029468 | p = 0.122

## don't know why they're not exactly the same, but any differences are negligent anyway

fe_state_model_3 <- plm(scaled_homicide_deviation ~ scaled_rainfall_deviation, 
                     data = merged_state_data %>% filter(year != 2025), 
                     index = c("year", "month", "ENTIDAD"), 
                     model = "within")
summary(fe_state_model_3)

# beta = -0.0031048 | p = 0.099

# Fixed Effects Model with interaction term (so each state has its own slope)
fe_int_state_model <- plm(scaled_homicide_deviation ~ scaled_rainfall_deviation * factor(ENTIDAD), 
                     data = merged_state_data %>% filter(year != 2025), 
                     index = c("year", "month"), 
                     model = "within")

summary(fe_int_state_model)

library(lme4)
lme_int_state_model <- lmer(scaled_homicide_deviation ~ scaled_rainfall_deviation + 
                             (scaled_rainfall_deviation | ENTIDAD) + factor(month) + factor(year),
                           data = merged_state_data %>% filter(year != 2025))

summary(lme_int_state_model)

# consistent results again!

######## CONTINUE FROM HERE





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







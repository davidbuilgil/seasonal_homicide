
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
  #filter(year >= 1997) %>%
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
    title = "(a) Deviation from Monthly Mean Rainfall by Year",
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
homicide_month <- homicide_month %>%
  mutate(
    month_name = factor(month.name[month], levels = month.name, ordered = TRUE)
  )

# Plot Deviations by Month
homicide_plot <- ggplot(homicide_month, aes(x = year, y = deviation)) +
  geom_line(color = "steelblue", alpha = 0.6) +  # Original data
  geom_smooth(method = "lm", se = TRUE, color = "black", linewidth = 1) +  # LOESS trend line
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # Reference line at zero
  facet_wrap(~month_name, ncol = 4) +  # 12 facets (3 rows x 4 columns)
  labs(
    title = "(b) Deviation from Monthly Mean Homicide by Year",
    x = "",
    y = "Deviation"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = -60, hjust = 0))
homicide_plot

# Plot two graphs together
ggarrange(rainfall_plot, homicide_plot)
ggsave(here('output/deviations_by_year_mexico.jpg'), width = 10, height = 5)

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
summary(fe_model)$coefficients["scaled_rainfall_deviation", "Estimate"]
summary(fe_model)$coefficients["scaled_rainfall_deviation", "Estimate"] + c(-2, 2) * summary(fe_model)$coefficients["scaled_rainfall_deviation", "Std. Error"]

# Check fixed effects model
fe_model_check <- lm(scaled_homicide_deviation ~ scaled_rainfall_deviation + factor(year) + factor(month), 
                     data = merged_data %>% filter(year != 2025))

summary(fe_model_check)
summary(fe_model_check)$coefficients["scaled_rainfall_deviation", "Estimate"]
summary(fe_model_check)$coefficients["scaled_rainfall_deviation", "Estimate"] + c(-2, 2) * summary(fe_model_check)$coefficients["scaled_rainfall_deviation", "Std. Error"]

# GLS Model with Autocorrelation
gls_model <- gls(scaled_homicide_deviation ~ scaled_rainfall_deviation, 
                 correlation = corAR1(form = ~ year | month), 
                 data = merged_data %>% filter(year != 2025))

gls_model_ar2 <- gls(
  scaled_homicide_deviation ~ scaled_rainfall_deviation,
  correlation = corARMA(p = 2, form = ~ year | month),
  data = merged_data %>% filter(year != 2025)
)

gls_model_ar3 <- gls(
  scaled_homicide_deviation ~ scaled_rainfall_deviation,
  correlation = corARMA(p = 3, form = ~ year | month),
  data = merged_data %>% filter(year != 2025)
)

summary(gls_model)$tTable["scaled_rainfall_deviation", "Value"]
summary(gls_model)$tTable["scaled_rainfall_deviation", "Value"] + c(-2, 2) * summary(gls_model)$tTable["scaled_rainfall_deviation", "Std.Error"]

summary(gls_model_ar2)$tTable["scaled_rainfall_deviation", "Value"]
summary(gls_model_ar2)$tTable["scaled_rainfall_deviation", "Value"] + c(-2, 2) * summary(gls_model_ar2)$tTable["scaled_rainfall_deviation", "Std.Error"]

summary(gls_model_ar3)$tTable["scaled_rainfall_deviation", "Value"]
summary(gls_model_ar3)$tTable["scaled_rainfall_deviation", "Value"] + c(-2, 2) * summary(gls_model_ar3)$tTable["scaled_rainfall_deviation", "Std.Error"]

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

summary(fe_state_model) # coefficients show how much municipalities contribute towards change in homicide
summary(fe_state_model)$coefficients["scaled_rainfall_deviation", "Estimate"]
summary(fe_state_model)$coefficients["scaled_rainfall_deviation", "Estimate"] + c(-2, 2) * summary(fe_state_model)$coefficients["scaled_rainfall_deviation", "Std. Error"]

# Checking model
fe_state_model_2 <- lm(scaled_homicide_deviation ~ scaled_rainfall_deviation + factor(ENTIDAD) + factor(year) + factor(month), 
                     data = merged_state_data %>% filter(year != 2025))

summary(fe_state_model_2)
summary(fe_state_model_2)$coefficients["scaled_rainfall_deviation", "Estimate"]
summary(fe_state_model_2)$coefficients["scaled_rainfall_deviation", "Estimate"] + c(-2, 2) * summary(fe_state_model_2)$coefficients["scaled_rainfall_deviation", "Std. Error"]

fe_state_model_3 <- plm(scaled_homicide_deviation ~ scaled_rainfall_deviation, 
                     data = merged_state_data %>% filter(year != 2025), 
                     index = c("year", "month", "ENTIDAD"), 
                     model = "within")

summary(fe_state_model_3)
summary(fe_state_model_3)$coefficients["scaled_rainfall_deviation", "Estimate"]
summary(fe_state_model_3)$coefficients["scaled_rainfall_deviation", "Estimate"] + c(-2, 2) * summary(fe_state_model_3)$coefficients["scaled_rainfall_deviation", "Std. Error"]

# Fixed Effects Model with interaction term (so each state has its own slope)
fe_int_state_model <- plm(scaled_homicide_deviation ~ scaled_rainfall_deviation * factor(ENTIDAD), 
                     data = merged_state_data %>% filter(year != 2025), 
                     index = c("year", "month"), 
                     model = "within")

summary(fe_int_state_model)

# Extract coefficients table
coeff_fe_int_table <- as.data.frame(coef(summary(fe_int_state_model)))

# Filter for ENTIDAD interaction terms only
coeff_fe_int_table <- coeff_fe_int_table[grep("scaled_rainfall_deviation:factor\\(ENTIDAD\\)", rownames(coeff_fe_int_table)), ]

# ENTIDAD identifiers
coeff_fe_int_table$ENTIDAD <- gsub("scaled_rainfall_deviation:factor\\(ENTIDAD\\)", "", rownames(coeff_fe_int_table))

# Load shapefile data
shapefile_data <- st_read(here("data/mexico-replication/mex_admbnda_adm1_govmex_20210618.shp"))

# Rename states
state_lookup3 <- tibble::tribble(
  ~ENTIDAD, ~state,
  "Aguascalientes", "AGUASCALIENTES",
  "Baja California", "BAJA CALIFORNIA",
  "Baja California Sur", "BAJA CALIFORNIA SUR",
  "Campeche", "CAMPECHE",
  "Coahuila de Zaragoza", "COAHUILA DE ZARAGOZA",
  "Colima", "COLIMA",
  "Chiapas", "CHIAPAS",
  "Chihuahua", "CHIHUAHUA",
  "Distrito Federal", "CIUDAD DE MEXICO",
  "Durango", "DURANGO",
  "Guanajuato", "GUANAJUATO",
  "Guerrero", "GUERRERO",
  "Hidalgo", "HIDALGO",
  "Jalisco", "JALISCO",
  "Mexico", "MEXICO",
  "Michoacan de Ocampo", "MICHOACAN",
  "Morelos", "MORELOS",
  "Nayarit", "NAYARIT",
  "Nuevo Leon", "NUEVO LEON",
  "Oaxaca", "OAXACA",
  "Puebla", "PUEBLA",
  "Queretaro de Arteaga", "QUERETARO",
  "Quintana Roo", "QUINTANA ROO",
  "San Luis Potosi", "SAN LUIS POTOSI",
  "Sinaloa", "SINALOA",
  "Sonora", "SONORA",
  "Tabasco", "TABASCO",
  "Tamaulipas", "TAMAULIPAS",
  "Tlaxcala", "TLAXCALA",
  "Veracruz de Ignacio de la Llave", "VERACRUZ",
  "Yucatan", "YUCATAN",
  "Zacatecas", "ZACATECAS"
)

shapefile_data <- shapefile_data %>%
  mutate(ADM1_ES = ifelse(!is.na(ADM1_REF), ADM1_REF, ADM1_ES)) %>%
  left_join(state_lookup3, by = c("ADM1_ES" = "ENTIDAD")) %>%
  rename(ENTIDAD = state)

# Left join estimates
shapefile_data <- shapefile_data %>%
  left_join(coeff_fe_int_table, by = "ENTIDAD")

# Plot regression estimates
estimates_map <- ggplot(data = shapefile_data) +
  geom_sf(aes(fill = sign(Estimate) * sqrt(abs(Estimate))), colour = NA) +
  scale_fill_gradient2(
    low = "darkblue",      # Color for negative values
    mid = "lightgrey",     # Neutral (zero) point
    high = "darkred",      # Color for positive values
    midpoint = 0,      # Zero is the neutral point
    na.value = "white",
    guide = guide_colorbar(title = "Estimate (sqrt)")
  ) +
  labs(title = "(e) State-Specific Rainfall Effects") +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    strip.text = element_text(size = 10, face = "bold"),
    legend.position = "bottom"
  )
estimates_map

# Plot two graphs together
ggarrange(rainfall_plot, homicide_plot,
          estimates_map,
          ncol = 2, nrow = 2,
          widths = c(1, 1),
          heights = c(1, 1.3))
ggsave(here('output/mexico_final.jpg'), width = 10, height = 10)

# Density lines rain per periods
periods <- list(
  "1981–1985" = 1981:1985,
  "1986–1990" = 1986:1990,  
  "1991–1995" = 1991:1995,
  "1996–2000" = 1996:2000,  
  "2001–2005" = 2001:2005,
  "2006–2010" = 2006:2010,
  "2011–2015" = 2011:2015,
  "2016–2020" = 2016:2020,
  "2021–2024" = 2021:2024
)

# Compute monthly means per period
period_means <- map_dfr(names(periods), function(p) {
  rainfall_month %>%
    filter(year %in% periods[[p]]) %>%
    group_by(month) %>%
    summarise(mean_rfh = mean(rfh, na.rm = TRUE), .groups = "drop") %>%
    mutate(period = p)
})

# Order periods old -> new
period_levels <- names(periods)
period_means <- period_means %>%
  mutate(period = factor(period, levels = period_levels))

# Blue -> red colour gradient
n_periods <- length(period_levels)
period_cols <- colorRampPalette(c("navy", "blue", "skyblue", "orange", "red"))(n_periods)
names(period_cols) <- period_levels

# Density plot (weighted by mean rainfall)
rain_period_means <- ggplot(period_means, aes(x = month, colour = period)) +
  geom_density(
    aes(weight = mean_rfh),
    linewidth = 1,
    adjust = 0.6,
    alpha = 0.9,
    key_glyph = "path"
  ) +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  scale_colour_manual(values = period_cols) +
  #coord_cartesian(ylim = c(0.025, 0.11)) +
  labs(
    title = "(a) Density of Monthly Rainfall Across Periods",
    x = "",
    y = "Density",
    colour = ""
  ) +
  theme_minimal() +
  theme(
    legend.key = element_blank()
  ) +
  guides(
    colour = guide_legend(
      override.aes = list(
        linetype = 1,
        size = 1.2,
        fill = NA
      )
    )
  )

# Density lines homicide per periods
periods <- list(
  "1997–2000" = 1997:2000,  
  "2001–2005" = 2001:2005,
  "2006–2010" = 2006:2010,
  "2011–2015" = 2011:2015,
  "2016–2020" = 2016:2020,
  "2021–2024" = 2021:2024
)

# Compute monthly means per period
period_means <- map_dfr(names(periods), function(p) {
  homicide_month %>%
    filter(year %in% periods[[p]]) %>%
    group_by(month) %>%
    summarise(mean_homicide = mean(total, na.rm = TRUE), .groups = "drop") %>%
    mutate(period = p)
})

# Order periods old -> new
period_levels <- names(periods)
period_means <- period_means %>%
  mutate(period = factor(period, levels = period_levels))

# Blue -> red colour gradient
n_periods <- length(period_levels)
period_cols <- colorRampPalette(c("navy", "blue", "skyblue", "orange", "red"))(n_periods)
names(period_cols) <- period_levels

# Density plot (weighted by mean rainfall)
homicide_period_means <- ggplot(period_means, aes(x = month, colour = period)) +
  geom_density(
    aes(weight = mean_homicide),
    linewidth = 1,
    adjust = 0.6,
    alpha = 0.9,
    key_glyph = "path"
  ) +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  scale_colour_manual(values = period_cols) +
  coord_cartesian(ylim = c(0.05, 0.09)) +
  labs(
    title = "(b) Density of Monthly Homicide Across Periods",
    x = "",
    y = "Density",
    colour = ""
  ) +
  theme_minimal() +
  theme(
    legend.key = element_blank()
  ) +
  guides(
    colour = guide_legend(
      override.aes = list(
        linetype = 1,
        size = 1.2,
        fill = NA
      )
    )
  )

# Plot five plots together
ggarrange(
  rain_period_means, homicide_period_means,
  rainfall_plot, homicide_plot,
  estimates_map,
  ncol = 2, nrow = 3,
  widths = c(1, 1),
  heights = c(0.7, 1, 1.3)
)

ggsave(here('output/mexico_final_5.jpg'), width = 10, height = 13)

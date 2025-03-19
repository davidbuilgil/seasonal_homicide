
rm(list = ls())

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

# rm(rainfall1, rainfall2, rainfall3)

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
    x = "Year",
    y = "Deviation"
  ) +
  theme_minimal()
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
    x = "Year",
    y = "Deviation"
  ) +
  theme_minimal()
homicide_plot

# Plot two graphs together
ggarrange(rainfall_plot, homicide_plot)

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

# Calculate municipality and monthly mean (after standardization)
homicide_muni_monthly_mean <- homicide_muni_month %>%
  group_by(month, COD_MUNI) %>%
  summarise(mean_standardized_homicide = mean(standardized_homicide, na.rm = TRUE))

# Calculate deviation from mean for each municipality and month
homicide_muni_month <- homicide_muni_month %>%
  left_join(homicide_muni_monthly_mean, by = c("month", "COD_MUNI")) %>%
  mutate(deviation = standardized_homicide - mean_standardized_homicide)

# Merge rainfall and homicide municipality data
merged_muni_data <- rainfall_muni_month %>%
  inner_join(homicide_muni_month, by = c("year", "month", "COD_MUNI")) %>%
  select(year, month, COD_MUNI, deviation.x, deviation.y) %>%
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

# Fixed Effects Model
fe_dept_model <- plm(scaled_homicide_deviation ~ scaled_rainfall_deviation, 
                     data = merged_muni_data %>% filter(year != 2025), 
                     index = c("COD_MUNI", "time"), 
                     model = "within")

summary(fe_dept_model)































###### IGNORE FROM HERE

# Plot facets by year
ggplot(rainfall_month, aes(x = month, y = rfh, group = year, color = factor(year))) +
  geom_line() +
  labs(title = "Seasonal Patterns in RFH by Year",
       x = "Month",
       y = "RFH") +
  theme_minimal()


library(lmtest)

# Fit a linear model with interaction
model <- lm(rfh ~ factor(month) * year, data = rainfall_month)

# Check significance of interaction
summary(model)



library(mgcv)

rainfall_month <- rainfall_month %>%
  mutate(time = year + (month - 1) / 12)

# Fit a GAM model with evolving seasonal patterns
gam_model <- gam(rfh ~ s(time) + s(time, bs = "cc", k = 12), data = rainfall_month)

# Plot the fitted model
plot(gam_model, pages = 1)

# GAM model with interaction for evolving seasonality
gam_model_interaction <- gam(rfh ~ s(time) + ti(month, time, bs = c("cc", "tp"), k = c(12, 5)),
                             data = rainfall_month)

# Plot the model
plot(gam_model_interaction, pages = 1)

# Compare models to assess if the interaction term significantly improves fit
anova(gam_model, gam_model_interaction, test = "Chisq")

# Visualise the interaction effect
plot(gam_model_interaction, select = 2, shade = TRUE)

gam.check(gam_model_interaction)



library(forecast)

# Convert to time series object (required for decomposition)
rfh_ts <- ts(rainfall_month$rfh, start = c(min(rainfall_month$year), min(rainfall_month$month)), frequency = 12)

# Decompose time series
decomp <- decompose(rfh_ts)

# Plot decomposition
plot(decomp)

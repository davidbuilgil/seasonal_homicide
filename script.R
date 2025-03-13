
# Load packages
library(here)
library(ggplot2)
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
         month = month(date)) %>%
  filter(year >= 2003)

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
  left_join(rainfall_monthly_mean, by = "month") %>%     # TO: changing 'monthly_mean' for 'rainfall_monthly_mean'
  mutate(
    deviation = rfh - mean_rfh,  # Deviation size
    above_below_avg = ifelse(deviation > 0, "Above", "Below")
  )

# Plot facets by month
rainfall_plot <- ggplot(rainfall_month, aes(x = year, y = deviation)) +
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

###########################

## THIAGO: For step 1 above, maybe we should use data since 1981 instead of 
#          since 2003? The goal, if I understood correctly, is simply to check
#          how each monthly rfh deviates from the overall mean for that month.
#          For that, calculating the overall mean since 1981 is better, I think
# Doing that below:

# Find month and year in date SINCE 1981
rainfall_since1981 <- rainfall %>%
  mutate(year = year(date),
         month = month(date))

# Aggregate data monthly
rainfall_month_since1981 <- rainfall_since1981 %>%
  group_by(month, year) %>%
  summarise(rfh = sum(rfh)) %>%
  mutate(month_name = factor(month.name[month], levels = month.name))

# Calculate monthly mean and deviantion from monthly mean across years
# Step 1: Calculate mean rfh for each month across years
rainfall_monthly_mean_since1981 <- rainfall_month_since1981 %>%
  group_by(month) %>%
  summarise(mean_rfh_since1981 = mean(rfh, na.rm = TRUE))

# Step 2: Identify deviation from the mean and direction
rainfall_month_new <- rainfall_month %>%
  # now we can keep the 2003-2025 dataset
  left_join(rainfall_monthly_mean_since1981, by = "month") %>%     
  # but comparing them against the 1981-2025 mean
  mutate(
    deviation_1981 = rfh - mean_rfh_since1981,  # Deviation size
    above_below_avg_1981 = ifelse(deviation_1981 > 0, "Above", "Below")
  )

# check correlation between the to deviation measures
rainfall_month_new %>% ungroup %>% dplyr::select(deviation, deviation_1981) %>% cor()
# r = 0.98, so no difference

# Plot facets by month
rainfall_plot_81 <- ggplot(rainfall_month_new, aes(x = year, y = deviation_1981)) +
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
rainfall_plot_81


###########################

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
 
# alternatively:
ggarrange(rainfall_plot_81, homicide_plot)




lm(deviation_homicide ~ deviation_rainfall, joint) %>% summary


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

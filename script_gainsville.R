
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
rainfall <- read.csv(here("data/gainesville-replication/monthly_values_2011_2024.csv"))

# Find month and year in date
rainfall <- rainfall %>%
  mutate(date = dmy(date),
         year = year(date),
         month = month(date))

# Aggregate data monthly
rainfall_month <- rainfall %>%
  mutate(month_name = factor(month.name[month], levels = month.name))

# Calculate monthly mean and deviantion from monthly mean across years
# Step 1: Calculate mean rfh for each month across years
rainfall_monthly_mean <- rainfall_month %>%
  group_by(month) %>%
  summarise(mean_value = mean(value, na.rm = TRUE))

# Step 2: Identify deviation from the mean and direction
rainfall_month <- rainfall_month %>%
  left_join(rainfall_monthly_mean, by = "month") %>%
  mutate(
    deviation = value - mean_value,  # Deviation size
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

# Load violence data
violence <- read.csv(here("data/gainesville-replication/Crime_Responses_20251111.csv"))

serious_violence_keywords <- c(
  "Homicide", "Battery", "Assault", "Robbery", "Affray", "With Violence",
  "False Imprisonment", "Extortion", "Child Molestation", "Striking a Police Animal"
)

violence <- violence %>%
  filter(str_detect(Incident.Type, str_c(serious_violence_keywords, collapse = "|")))

cyber_terms <- c(
  "Cyber", "Computer", "Fraud", "Calls", "Written Threat", "Stalking", "Sexting"
)

violence <- violence %>%
  filter(!str_detect(Incident.Type, str_c(cyber_terms, collapse = "|")))

domestic_terms <- c(
  "Domestic", "Dating", "Non-reporting Sexual Battery Kit"
)

violence <- violence %>%
  filter(!str_detect(Incident.Type, str_c(domestic_terms, collapse = "|")))

# Extract year and month from the Report.Date
violence <- violence %>%
  mutate(
    date = gsub("\\s+\\d{1,2}:\\d{2}:\\d{2}\\s*(AM|PM)", "", Report.Date),
    date = mdy(date),
    year = year(date),
    month = month(date)
  )

# Aggregate data monthly
violence_month <- violence %>%
  mutate(value = 1) %>%
  group_by(year, month) %>%
  summarise(violence = sum(value, na.rm = TRUE), .groups = "drop") %>%
  tidyr::complete(
    year = 2011:2024,
    month = 1:12,
    fill = list(violence = 0)
  ) %>%
  mutate(month_name = factor(month.name[month], levels = month.name)) %>%
  arrange(year, month)

# Standardize violence counts within each year
violence_month <- violence_month %>%
  group_by(year) %>%
  mutate(standardized_violence = scale(violence)[, 1]) %>%
  ungroup()

# Calculate monthly mean (after standardization)
violence_monthly_mean <- violence_month %>%
  group_by(month) %>%
  summarise(mean_standardized_violence = mean(standardized_violence, na.rm = TRUE))

# Calculate deviation from mean for each month
violence_month <- violence_month %>%
  left_join(violence_monthly_mean, by = "month") %>%
  mutate(deviation = standardized_violence - mean_standardized_violence)

# Plot Deviations by Month
violence_plot <- ggplot(violence_month, aes(x = year, y = deviation)) +
  geom_line(color = "steelblue", alpha = 0.6) +  # Original data
  geom_smooth(method = "lm", se = TRUE, color = "black", linewidth = 1) +  # LOESS trend line
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # Reference line at zero
  facet_wrap(~month_name, ncol = 4) +  # 12 facets (3 rows x 4 columns)
  labs(
    title = "Deviation from Monthly Mean Violence by Year",
    x = "",
    y = "Deviation"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = -60, hjust = 0))
violence_plot

# Plot two graphs together
ggarrange(rainfall_plot, violence_plot)
ggsave(here('output/two_plots_gainesville.jpg'), width = 10, height = 5)

# Merge rainfall and violence data
merged_data <- rainfall_month %>%
  inner_join(violence_month, by = c("year", "month")) %>%
  select(year, month, deviation.x, deviation.y) %>%
  rename(rainfall_deviation = deviation.x,
         violence_deviation = deviation.y)

# Rescale all variables of interest
merged_data <- merged_data %>%
  mutate(scaled_rainfall_deviation = scale(rainfall_deviation)[, 1],
         scaled_violence_deviation = scale(violence_deviation)[, 1])

# Fixed Effects Model
fe_model <- plm(scaled_violence_deviation ~ scaled_rainfall_deviation, 
                data = merged_data %>% filter(year != 2025), 
                index = c("year", "month"), 
                model = "within")

summary(fe_model)
summary(fe_model)$coefficients["scaled_rainfall_deviation", "Estimate"]
summary(fe_model)$coefficients["scaled_rainfall_deviation", "Estimate"] + c(-2, 2) * summary(fe_model)$coefficients["scaled_rainfall_deviation", "Std. Error"]

# Check fixed effects model
fe_model_check <- lm(scaled_violence_deviation ~ scaled_rainfall_deviation + factor(year) + factor(month), 
                data = merged_data %>% filter(year != 2025))

summary(fe_model_check)
summary(fe_model_check)$coefficients["scaled_rainfall_deviation", "Estimate"]
summary(fe_model_check)$coefficients["scaled_rainfall_deviation", "Estimate"] + c(-2, 2) * summary(fe_model_check)$coefficients["scaled_rainfall_deviation", "Std. Error"]

# GLS Model with Autocorrelation
gls_model <- gls(scaled_violence_deviation ~ scaled_rainfall_deviation, 
                 correlation = corAR1(form = ~ year | month), 
                 data = merged_data %>% filter(year != 2025))

gls_model_ar2 <- gls(
  scaled_violence_deviation ~ scaled_rainfall_deviation,
  correlation = corARMA(p = 2, form = ~ year | month),
  data = merged_data %>% filter(year != 2025)
)

gls_model_ar3 <- gls(
  scaled_violence_deviation ~ scaled_rainfall_deviation,
  correlation = corARMA(p = 3, form = ~ year | month),
  data = merged_data %>% filter(year != 2025)
)

summary(gls_model)$tTable["scaled_rainfall_deviation", "Value"]
summary(gls_model)$tTable["scaled_rainfall_deviation", "Value"] + c(-2, 2) * summary(gls_model)$tTable["scaled_rainfall_deviation", "Std.Error"]

summary(gls_model_ar2)$tTable["scaled_rainfall_deviation", "Value"]
summary(gls_model_ar2)$tTable["scaled_rainfall_deviation", "Value"] + c(-2, 2) * summary(gls_model_ar2)$tTable["scaled_rainfall_deviation", "Std.Error"]

summary(gls_model_ar3)$tTable["scaled_rainfall_deviation", "Value"]
summary(gls_model_ar3)$tTable["scaled_rainfall_deviation", "Value"] + c(-2, 2) * summary(gls_model_ar3)$tTable["scaled_rainfall_deviation", "Std.Error"]


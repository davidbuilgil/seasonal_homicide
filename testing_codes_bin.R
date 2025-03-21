# Create spatial weights matrix (Queen contiguity)
#nb <- poly2nb(merged_muni_data_5y %>% filter(month == 1)) 
nb <- poly2nb(merged_muni_data_5y) 
lw <- nb2listw(nb, style = "W", zero.policy = TRUE)
#saveRDS(nb, here("output/nb_object.rds"))
#saveRDS(lw, here("output/lw_object.rds"))
#lw <- readRDS(here("output/lw_object.rds"))

# Replace NAs with 0 for visualization
merged_muni_data_5y <- merged_muni_data_5y %>%
  mutate(mean_rainfall_deviation = ifelse(is.na(mean_rainfall_deviation), 0, mean_rainfall_deviation),
         mean_homicide_deviation = ifelse(is.na(mean_homicide_deviation), 0, mean_homicide_deviation))

# Compute LISA for rainfall deviation
lisa_rainfall <- localmoran(merged_muni_data_5y$mean_rainfall_deviation, lw, zero.policy = TRUE)

# Add LISA values to dataset
merged_muni_data_5y <- merged_muni_data_5y %>%
  ungroup() %>%
  mutate(
    moran_I_rain = as.vector(lisa_rainfall[, 1]),
    p_value_rain = as.vector(lisa_rainfall[, 5])
  )

# Compute LISA for rainfall deviation each month
merged_muni_data_5y <- merged_muni_data_5y %>%
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
lisa_homicide <- localmoran(merged_muni_data_5y$mean_homicide_deviation, lw, zero.policy = TRUE)

# Add LISA values to dataset
merged_muni_data_5y <- merged_muni_data_5y %>%
  mutate(
    moran_I_homicide = lisa_homicide[, 1],
    p_value_homicide = lisa_homicide[, 5]
  )

# Assign clusters using global thresholds
merged_muni_data_5y <- merged_muni_data_5y %>%
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
homicide_map <- ggplot(merged_muni_data_5y) +
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
rainfall_map <- ggplot(merged_muni_data_5y) +
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

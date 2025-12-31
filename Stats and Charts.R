library(e1071) 
library(dplyr)
library(ggcorrplot)



df <- read.csv("Weather_data_2.csv")
df$date <- as.Date(df$date)
df$time_num <- as.numeric(df$date)
df$time_index <- seq_len(nrow(df))

####################################################################################3

date_range <- range(df$date, na.rm = TRUE)
cat("Data Starts on:", as.character(date_range[1]), "\n")
cat("Data Ends on  :", as.character(date_range[2]), "\n")
cat("Total Duration:", difftime(date_range[2], date_range[1], units = "days"), "days\n")

######################################################################################

all_weather_cols <- c("acc_precip", "bright_sunshine", "max_temp_w_date", 
                      "max_wind_speed_10min", "max_wind_speed_3sec", "mean_pressure", 
                      "mean_radiation", "mean_relative_hum", "mean_temp", 
                      "mean_wind_dir", "mean_wind_speed", "min_temp", 
                      "vapour_pressure_deficit_mean")

get_full_stats <- function(column_name, data) {
  x <- data[[column_name]]
  data.frame(
    Variable = column_name,
    Mean     = mean(x, na.rm = TRUE),
    Median   = median(x, na.rm = TRUE),
    Min      = min(x, na.rm = TRUE),
    Max      = max(x, na.rm = TRUE),
    SD       = sd(x, na.rm = TRUE),
    Skewness = skewness(x, na.rm = TRUE)
  )
}

full_stats_summary <- do.call(rbind, lapply(all_weather_cols, get_full_stats, data = df))

print(full_stats_summary, digits = 3)

##################################################################################

breaks <- c(-0.001, 0, 1, 5, 10, 15, 20, Inf)
labels <- c("0 (Dry)", "0-1 (Trace)", "1-5 (Light)", "5-10 (Moderate)", 
            "10-15 (Heavy)", "15-20 (Very Heavy)", "20+ (Extreme)")

precip_bins <- df %>%
  mutate(bin = cut(acc_precip, breaks = breaks, labels = labels)) %>%
  group_by(bin) %>%
  summarise(
    Count = n(),
    Percentage = round((n() / nrow(df)) * 100, 2)
  )

print(precip_bins)

#################################################################################
df_untrim <- df
df <- subset(df, df$acc_precip < 15)

################################################################################
all_weather_cols <- c("acc_precip", "bright_sunshine", "max_temp_w_date", 
                      "max_wind_speed_10min", "max_wind_speed_3sec", "mean_pressure", 
                      "mean_radiation", "mean_relative_hum", "mean_temp", 
                      "mean_wind_dir", "mean_wind_speed", "min_temp", 
                      "vapour_pressure_deficit_mean")

get_full_stats <- function(column_name, data) {
  x <- data[[column_name]]
  data.frame(
    Variable = column_name,
    Mean     = mean(x, na.rm = TRUE),
    Median   = median(x, na.rm = TRUE),
    Min      = min(x, na.rm = TRUE),
    Max      = max(x, na.rm = TRUE),
    SD       = sd(x, na.rm = TRUE),
    Skewness = skewness(x, na.rm = TRUE)
  )
}

full_stats_summary <- do.call(rbind, lapply(all_weather_cols, get_full_stats, data = df))

print(full_stats_summary, digits = 3)

#################################################################################

weather_data <- df %>%
  select(acc_precip, bright_sunshine, max_temp_w_date, 
         max_wind_speed_10min, max_wind_speed_3sec, mean_pressure, 
         mean_radiation, mean_relative_hum, mean_temp, 
         mean_wind_dir, mean_wind_speed, min_temp, 
         vapour_pressure_deficit_mean)

corr_matrix <- cor(weather_data, use = "complete.obs")

ggcorrplot(corr_matrix, 
           hc.order = TRUE,           
           type = "lower",            
           lab = TRUE,                
           lab_size = 3,              
           method = "square",         
           colors = c("#6D9EC1", "white", "#E46726"),
           title = "Correlation Heatmap of Meteorological Variables",
           ggtheme = theme_minimal()) + 
           theme(
             axis.text.x = element_text(color = "black", size = 10, angle = 45, vjust = 1, hjust = 1), # Darkens & tilts bottom labels
             axis.text.y = element_text(color = "black", size = 10),                                  # Darkens side labels
             plot.title = element_text(face = "bold", size = 14)
           )

################################################################################

df$month <- format(df$date, "%m")

# We add a small constant (0.1) because log(0) is undefined
ggplot(df, aes(x = month, y = acc_precip + 0.1, fill = month)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 1, alpha = 0.5) +
  scale_y_log10() +
  labs(title = "Seasonal Precipitation Variance (Log Scale)",
       subtitle = "Logarithmic scaling reveals the structural differences in rainfall hidden by the zero-spike.",
       x = "Month", y = "Precipitation (mm + 0.1, Log Scale)") +
  theme_minimal() +
  theme(legend.position = "none")

################################################################################

zero_count <- mean(df$acc_precip == 0, na.rm = TRUE) * 100

ggplot(df, aes(x = acc_precip)) +
  geom_histogram(fill = "#7fcdbb", bins = 50, color = "white") +
  coord_cartesian(xlim = c(0, 2.25)) + 
  labs(title = "Precipitation Distribution: The Zero-Spike", 
       subtitle = paste0("Approximately ", round(zero_count, 1), "% of observations are recorded as 0.0 mm."),
       x = "Daily Precipitation (mm)", y = "Frequency Count") +
  theme_minimal()

################################################################################
heavy_rain_days <- df_untrim %>% 
  filter(acc_precip > 15)

print(heavy_rain_days)
#### 0. Install & load libraries (run once) ####
required <- c("tidyverse","lubridate","sf","dbscan","leaflet","viridis","ggplot2","hexbin","scales")
new <- setdiff(required, installed.packages()[,"Package"])
if(length(new)) install.packages(new)
library(tidyverse); library(lubridate); library(sf); library(dbscan)
library(leaflet); library(viridis); library(ggplot2); library(hexbin); library(scales)

#### 1. Get data: Option A = read your CSV; Option B = create sample dataset ####
# Option A: If you downloaded a CSV, put its filename here and uncomment:
# accidents <- read_csv("your_accident_file.csv")

# Option B: Create a small realistic sample dataset (2000 rows)
set.seed(42)
n <- 2000
lat_min <- 40.60; lat_max <- 40.92
lon_min <- -74.10; lon_max <- -73.70
accidents <- tibble(
  id = 1:n,
  datetime = as.POSIXct("2024-01-01 00:00:00", tz = "UTC") + runif(n, 0, 365*24*3600),
  latitude = runif(n, lat_min, lat_max) + rnorm(n, 0, 0.003),
  longitude = runif(n, lon_min, lon_max) + rnorm(n, 0, 0.003),
  severity = sample(1:4, n, replace = TRUE, prob = c(0.6,0.25,0.1,0.05)),
  vehicles_involved = pmin(6, pmax(1, rpois(n, 2))),
  injuries = rbinom(n, size = 3, prob = 0.12)
)
# add weather/road/light correlated with month/time
accidents <- accidents %>%
  mutate(
    month = month(datetime),
    hour = hour(datetime),
    weekday = wday(datetime, label = TRUE),
    weather = case_when(
      month %in% c(12,1,2) & runif(n) < 0.25 ~ sample(c("Snow","Icy"), n, replace = TRUE),
      runif(n) < 0.15 ~ "Rain",
      runif(n) < 0.03 ~ "Fog",
      runif(n) < 0.05 ~ "Windy",
      TRUE ~ "Clear"
    ),
    road_condition = case_when(
      weather %in% c("Snow") ~ "Snow-covered",
      weather %in% c("Rain") ~ "Wet",
      runif(n) < 0.02 ~ "Icy",
      TRUE ~ "Dry"
    ),
    light_condition = case_when(
      hour >= 6 & hour < 18 ~ "Daylight",
      hour %in% c(5,18) ~ "Dawn/Dusk",
      TRUE ~ sample(c("Dark - Lighted","Dark - Not Lighted"), n, replace = TRUE, prob = c(0.8,0.2))
    )
  )

#### 2. Quick data checks & cleaning ####
glimpse(accidents)
# look for NAs
colSums(is.na(accidents))
# if you have missing lat/lon or datetime, remove or impute:
accidents <- accidents %>% filter(!is.na(latitude) & !is.na(longitude) & !is.na(datetime))

#### 3. Feature engineering ####
accidents <- accidents %>%
  mutate(
    date = as.Date(datetime),
    time_of_day = case_when(
      hour %in% 0:4 ~ "LateNight",
      hour %in% 5:7 ~ "MorningRush",
      hour %in% 8:15 ~ "Daytime",
      hour %in% 16:18 ~ "EveningRush",
      hour %in% 19:23 ~ "Night",
      TRUE ~ "Other"
    )
  )

#### 4. Simple EDA: time-of-day and weekday patterns ####
# accidents by hour
acc_by_hour <- accidents %>% count(hour)
ggplot(acc_by_hour, aes(x = hour, y = n)) +
  geom_line() + geom_point() +
  labs(title = "Accidents by hour", x = "Hour", y = "Count") +
  theme_minimal()

# accidents by time_of_day
accidents %>% count(time_of_day) %>%
  mutate(time_of_day = fct_relevel(time_of_day, "LateNight","MorningRush","Daytime","EveningRush","Night")) %>%
  ggplot(aes(x = time_of_day, y = n)) + geom_col() +
  labs(title = "Accidents by time of day", x = "", y = "Count") + theme_minimal()

# accidents by weekday
accidents %>% count(weekday) %>%
  ggplot(aes(x = weekday, y = n)) + geom_col() +
  labs(title = "Accidents by weekday", x = "", y = "Count") + theme_minimal()

#### 5. EDA: weather & road conditions ####
accidents %>% count(weather) %>% arrange(desc(n))
ggplot(accidents %>% count(weather), aes(x = reorder(weather,n), y = n)) +
  geom_col() + coord_flip() + labs(title = "Accidents by weather", x = "", y = "Count")

accidents %>% count(road_condition) %>% arrange(desc(n))
ggplot(accidents %>% count(road_condition), aes(x = reorder(road_condition,n), y = n)) +
  geom_col() + coord_flip() + labs(title = "Accidents by road condition", x = "", y = "Count")

#### 6. Spatial visuals: scatter + hex-bin density ####
ggplot(accidents, aes(x = longitude, y = latitude)) +
  geom_point(alpha = 0.4, size = 0.9) + coord_quickmap() +
  labs(title = "Accident locations (scatter)")

ggplot(accidents, aes(x = longitude, y = latitude)) +
  stat_bin_hex(bins = 50) +
  scale_fill_viridis_c(trans = "sqrt") +
  coord_quickmap() +
  labs(title = "Accident density (hex bins)", fill = "count")

#### 7. Interactive map with Leaflet ####
# convert to sf for convenience (not required for leaflet)
acc_sf <- st_as_sf(accidents, coords = c("longitude","latitude"), crs = 4326, remove = FALSE)

leaflet(acc_sf) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addCircleMarkers(~longitude, ~latitude, radius = 4, stroke = FALSE, fillOpacity = 0.6,
                   color = ~ifelse(severity>=3,"red","blue"),
                   popup = ~paste0("<b>ID:</b> ", id, "<br><b>Date:</b> ", date,
                                   "<br><b>Weather:</b> ", weather, "<br><b>Road:</b> ", road_condition)) %>%
  addMarkers(clusterOptions = markerClusterOptions()) -> my_map
# In RStudio the map will render in the Viewer. Print object to view:
my_map

#### 8. Hotspot detection: DBSCAN (simple) ####
# DBSCAN works on numeric matrix
coords <- accidents %>% select(longitude, latitude) %>% as.matrix()
# eps is in degrees. 0.01 deg ≈ ~1 km (depends on latitude) — adjust as needed.
db <- dbscan(coords, eps = 0.01, minPts = 10)
accidents$cluster <- factor(ifelse(db$cluster == 0, NA, db$cluster))  # 0 = noise

# summary of clusters
table(db$cluster)
# Plot clusters
ggplot(accidents, aes(x = longitude, y = latitude, color = cluster)) +
  geom_point(alpha = 0.7, size = 1.2) + coord_quickmap() +
  scale_color_viridis_d(na.value = "grey80") +
  labs(title = "DBSCAN clusters (hotspots)")

#### 9. Analyze cluster contributing factors ####
cluster_summary <- accidents %>%
  filter(!is.na(cluster)) %>%
  group_by(cluster) %>%
  summarise(
    count = n(),
    mean_severity = mean(severity),
    top_weather = names(sort(table(weather), decreasing = TRUE))[1],
    top_road = names(sort(table(road_condition), decreasing = TRUE))[1],
    top_time = names(sort(table(time_of_day), decreasing = TRUE))[1],
    .groups = "drop"
  ) %>%
  arrange(desc(count))
print(cluster_summary)

#### 10. Save outputs ####
write_csv(cluster_summary, "cluster_summary.csv")
write_csv(accidents, "accidents_enhanced.csv")  # dataset with engineered fields
ggsave("accidents_by_hour.png", last_plot(), width = 8, height = 5) # saves last ggplot

#### End of script ####

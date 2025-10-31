# Installiation & Loading of Required Packages
install.packages("janitor")
install.packages("here")
install.packages("skimr")
install.packages("dplyr")
install.packages("ggplot2")
library(janitor)
library(here)
library(tidyverse)
library(lubridate)
library(skimr)
library(dplyr)
library(ggplot2)


# Importation of Data
raw19 <- read_csv(here("raw_data","Divvy_Trips_2019_Q1 - Divvy_Trips_2019_Q1.csv"))
raw20 <- read_csv(here("raw_data","Divvy_Trips_2020_Q1 - Divvy_Trips_2020_Q1.csv"))

#Standardise - Adjusting Column Names for Uniformity
p19 <- raw19 %>% 
  rename(
    ride_id=trip_id, started_at=start_time, ended_at=end_time,
    start_station_name=from_station_name, start_station_id=from_station_id,
    end_station_name=to_station_name, end_station_id=to_station_id,
    member_casual=usertype
  ) %>% 
  mutate(member_casual = recode(tolower(member_casual),
                                "subscriber"="member", "customer"="casual"))

p20 <- raw20 %>% 
  mutate(member_casual = recode(tolower(member_casual),
                                "subscriber"="member", "customer"="casual"))
p19 <- p19 %>% 
  mutate(ride_id = as.character(ride_id))

# Combining Tables
trips <-bind_rows(
  p19 %>% select(ride_id, started_at, ended_at, start_station_id, end_station_id,
                 start_station_name, end_station_name, member_casual,),
  p20 %>% select(ride_id, started_at, ended_at, start_station_id, end_station_id,
                 start_station_name, end_station_name, start_lat, start_lng, member_casual)
)

# Creating Column for Ride Length. Weekday 1 = Sunday, 7 = Saturday
trips_clean <- trips %>% 
  mutate(
    started_at = ymd_hms(started_at, quiet = TRUE),
    ended_at = ymd_hms(ended_at, quiet = TRUE),
    ride_length_sec = as.numeric(difftime(ended_at, started_at, units = "secs")),
    ride_length_mins = ride_length_sec/60,
    day_of_week = wday(started_at, week_start = 1, label = TRUE),
    day_of_week = factor(day_of_week,
                         levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")),
    month = month(started_at, label = TRUE),
    hour = hour(started_at)
  ) %>% 
  filter(!is.na(started_at), !is.na(ended_at),
        ride_length_mins > 1, ride_length_mins < 720) %>% # Removes Implausible Trips
  drop_na(member_casual)

### Data Has Been Processed
# Mean Trip Length
trips_clean %>% 
  summarise(avg_ride = mean(ride_length_mins, na.rm = TRUE))
# Max Trip Length
trips_clean %>% 
  summarise(max_ride = max(ride_length_mins))
# Mean & Max Trip Length by Customer Type
mean_max <- trips_clean %>% 
  group_by(member_casual) %>%
  summarise(
    mean_duration = round(mean(ride_length_mins, na.rm= TRUE), 2),
    max_duration = max(ride_length_mins)
)
### Calculating Average Trip Duration of each Customer Type
day_duration <- trips_clean %>% 
  group_by(member_casual, day_of_week) %>% 
  summarise(
    mean_duration = round(mean(ride_length_mins, na.rm = TRUE), 2)
  )

# Visualisation of Average Trip Duration by Customer Type
mean_max %>% 
  ggplot(aes(x=member_casual, y=mean_duration)) +
  geom_col(width=0.85, colour = "black", fill = "steelblue") +
  geom_text(data=mean_max, aes(label=paste0(mean_duration, " mins")),
            vjust = -0.6, size=3, show.legend = FALSE) +
  labs(
    title = "Average Trip Duration by Member Type",
    subtitle = "Casual members longer trips suggest recreational usage, Members appear to be commuters",
    x = "Customer",
    y = "Average Trip Duration (mins)"
  ) +
  theme_minimal(base_size = 12)


### Visualisation of Average Trip Duration across the week by Customer Type
day_duration %>% 
  ggplot(aes(x=day_of_week, y=mean_duration)) +
  geom_col(width=0.85, fill = "steelblue") +
  facet_wrap(~member_casual, nrow = 1, scales = "free_y") +
  labs(
    title = "Average Trip Duration by Customer Type",
    x = "Day of Week",
    y = "Duration (minutes)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    strip.text = element_text(face = "bold")
  )

# Determining Demand by Day of Week by Customer Type
day_counts <- trips_clean %>% 
  count(member_casual, day_of_week, name = "day_volume")

# Determing Usage by Hour
hour_counts <- trips_clean %>% 
  count(hour, name = "hour_volume")


# Histogram Displaying Holistic Usage per Hour
trips_clean %>% 
  ggplot(aes(x=hour)) +
  geom_histogram(binwidth = 1, fill = "steelblue", colour = "white") +
  scale_x_continuous(breaks=0:23) + 
  labs(
    title = "Distribution of Bike Usage by Hour",
    subtitle = "Histogram",
    x = 'Hour of Day',
    y = "Count"
  ) +
  theme(
    axis.text.x = element_text(angle=20,hjust=1, size= 6, face="bold")
  )
  theme_minimal()

# Determining Hourly Usage by Member Type:
hourly_usage <- trips_clean %>% 
  count(member_casual, hour, name = "hour_vol")
# Identifying Peak Usage Hours by Member Type:
peak <- hourly_usage %>% 
  group_by(member_casual) %>% 
  slice_max(hour_vol,n=1)

hour_breaks <- c(0,3,6,9,12,15,18,21)
hour_labels <- c("12am","3am","6am","9am","12pm","3pm","6pm","9pm")
hour_boundaries <- c(0,23,1)


# Visualisation Showing Usage Hours by Member Type
hourly_usage %>% 
  ggplot(aes(x=hour,y=hour_vol, colour = member_casual)) +
  geom_line(linewidth = 1) +
  facet_wrap(~member_casual, nrow=1, scales = "free_y") +
  geom_vline(xintercept = c(8,17), linetype = "dashed", alpha = 0.4) +
  geom_point(data = peak, size = 2) +
  geom_text(data=peak, aes(label=paste0(hour, ":00")),
            vjust = -0.6, size=3, show.legend = FALSE) +
  scale_x_continuous(breaks = hour_breaks, labels = hour_labels, limits = c(0,23)) +
  scale_y_continuous(labels=scales::comma) +
  labs(
    title = "Hourly Rider Demand: Members vs Casuals",
    subtitle = "Typical Commute Hours Marked",
    x = "Hour of Day (24h)",
    y = "Number of Rides",
    color = "Rider Type"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
      legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Visualisation of Most Common Days of Use - Members vs Casual
day_counts %>% 
ggplot(aes(x = day_of_week, y = day_volume)) +
  geom_col(width = 0.85, fill = "steelblue") +
  facet_wrap(~ member_casual, nrow = 1, scales = "free_y") +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Ride Volume by Day of Week",
    x = "Day of Week",
    y = "Number of Rides",
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    strip.text = element_text(face = "bold")
  )

# Determining Usage Hotspots
start_station_vol <- trips_clean %>% 
  filter(!is.na(start_station_name), start_station_name != "") %>% 
  count(member_casual, start_station_name, name = "station_vol")

# Joining Station Coordinates from p20 Dataset onto the Combined Dataset
## Determine Each Station's Coordinates:
station_lookup_id <- p20 %>%
  filter(!is.na(start_station_id), !is.na(start_lat), !is.na(start_lng)) %>%
  group_by(start_station_id) %>%
  summarise(
    start_lat_lookup = mean(as.numeric(start_lat), na.rm = TRUE),
    start_lng_lookup = mean(as.numeric(start_lng), na.rm = TRUE),
    .groups = "drop"
  )
## Join Coordinates onto Combined Dataset:
trips <- trips %>% 
  left_join(station_lookup_id, by = "start_station_id") %>%
  mutate(
    start_lat = dplyr::coalesce(as.numeric(start_lat), start_lat_lookup),
    start_lng = dplyr::coalesce(as.numeric(start_lng), start_lng_lookup)
  ) %>% 
  select(-start_lat_lookup, -start_lng_lookup)

# Load Trips clean again

# Determine Station Usage by Customer Type:
start_station_volumes <- trips_clean %>% 
  filter(!is.na(start_station_name), start_station_name != "") %>% 
  count(member_casual, start_station_name, start_lat, start_lng, name = "station_volumes")

# Identifying Most Popular Stations by Customer Type:
top_stations <- start_station_volumes %>% 
  group_by(member_casual) %>% 
  slice_max(station_volumes, n= 15, with_ties = FALSE) %>% 
  ungroup()

library(leaflet.extras)
library(geosphere)
library(htmlwidgets)
library(leaflet)
library(scales)

pal <- colorFactor(
  palette = c("goldenrod", "steelblue"),
  domain  = c("casual", "member")
)

peaks <-start_station_volumes %>% 
  filter(!is.na(start_lat), !is.na(start_lng)) %>% 
  group_by(member_casual) %>% 
  slice_max(station_volumes, n=1)


# Adding Tourist Hotspots onto Map
hotspots_raw <- read_csv(here("raw_data","chicago_hotspots.csv")) %>%
  clean_names()

hotspots <- hotspots_raw %>% 
  transmute(
    name = chicago_tourist_hot_spot,
    lat = as.numeric(lat),
    lng = as.numeric(long))
### Calculating Nearest Tourist Hotpot to each Top Station
nearest_hotspot <- top_stations %>%
  select(member_casual, start_station_name, start_lat, start_lng) %>%
  tidyr::crossing(
    hotspots %>% select(hotspot_name = name, lat, lng)
  ) %>%
  mutate(
    dist_m = geosphere::distHaversine(
      cbind(start_lng, start_lat),
      cbind(lng, lat)
    )
  ) %>%
  group_by(member_casual, start_station_name, start_lat, start_lng) %>%
  slice_min(dist_m, n = 1, with_ties = FALSE) %>%
  ungroup()
### Calculating % of Top Stations within xm of A Tourist Hotspot & Summary Statistics
nearest_by_type <- nearest_hotspot %>%
  group_by(member_casual) %>%
  summarise(
    n_stations      = dplyr::n(),
    mean_dist_m     = mean(dist_m),
    median_dist_m   = median(dist_m),
    pct_within_125m = mean(dist_m <= 125)* 100,
    pct_within_250m = mean(dist_m <= 250) * 100,
    pct_within_500m = mean(dist_m <= 500) * 100,
    pct_within_1km  = mean(dist_m <= 1000) * 100,
    .groups = "drop"
  )
### Making Info Popup for Graph Displaying Key Summary Statistics
cas_med <- nearest_by_type %>% 
  filter(member_casual=="casual") %>% 
  pull(median_dist_m) %>% 
  round(2)

mem_med <- nearest_by_type %>% 
  filter(member_casual=="member") %>% 
  pull(median_dist_m) %>% 
  round(2)

cas_250 <- nearest_by_type %>% 
  filter(member_casual=="casual") %>% 
  pull(pct_within_250m) %>% 
  round(2)

mem_250 <- nearest_by_type %>% 
  filter(member_casual=="member") %>% 
  pull(pct_within_250m) %>% 
  round(2)

###Building HTML for the on-map Info Panel:
stats_html <- htmltools::HTML(
  sprintf(
    "<div style='background:white; padding:8px 10px; border-radius: 6px;
    box-shadow:0 1px 6px rgba(0,0,0,.18); font-size:13px;line-height:1.5'>
    <div style='font-weight:700; margin-bottom:4px; color:#333;'>Tourist Hotspot Proximity:</div>
    <div style='margin-bottom:6px;'>
    <span style='color:#555; width:70px;font-weight:600;'>Casual:</span><br/>
    <span style='color#000;'>Median Distance: <b>%s m</b></span><br/>
    <span style='color:#555; '>Within 250 m: <b>%s%%</b></span>
    </div>
    <div style='border-top:1px solid #eee; padding-topL:6px;'>
    <span style='color:#555; width:70px;font-weight:600;'>Members:</span><br/>
    <span style='color#000;'>Median Distance: <b>%s m</b></span><br/>
    <span style='color:#555; '>Within 250 m: <b>%s%%</b></span>
    </div>
    </div>",
    round(cas_med,1), round(cas_250,1), round(mem_med,1), round(mem_250,1)
  )
)

### Creating Interactive Map Visualisation
map <- leaflet(top_stations) %>% 
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addCircles(
    data = peaks,
    lng = ~start_lng, lat = ~start_lat,
    radius = 70,
    color = "black",
    weight = 2,
    fillOpacity = 0,
    options = pathOptions(interactive = FALSE),
    group = "Most Popular Station") %>% 
  addCircleMarkers(
    lng = ~start_lng,
    lat = ~start_lat,
    radius = ~scales::rescale(station_volumes, to=c(5,20)),
    color = ~pal(member_casual),
    fillColor = ~pal(member_casual),
    fillOpacity = 0.7,
    stroke =TRUE, weight = 1,
    popup = ~paste0(
      "<b>", htmltools::htmlEscape(start_station_name), "</b><br/>",
      "Customer type: <b>", member_casual, "</b><br/>",
      "Rides: <b>", comma(station_volumes), "</b>"
    ),
    group = "Top Stations" 
    ) %>% 
  addHeatmap(
    data = hotspots,
    lng = ~lng, lat = ~lat,
    radius = 45,
    blur = 10,
    max = 2,
    group = "Tourist Hotspots"
  ) %>% 
  addLayersControl(
    overlayGroups = c("Top Stations", "Most Popular Station", "Tourist Hotspots"),
    options = layersControlOptions(collapsed=FALSE)
  ) %>% 
  addEasyButton(
    easyButton(
      icon = "fa-info", title = "Tourism Summary",
      onClick = JS(
        sprintf(
          "function(btn,map){
          var sz = map.getSize();
          var pt = L.point(120, sz.y - 10);
          var ll = map.containerPointToLatLng(pt)
          var html = `%s`;
          L.popup({
          autoPan: false,
          closeButton: true,
          offset: L.point(10,-10),
          maxWidth: 220,
          className: 'corner-popup'
          })
          .setLatLng(ll)
          .setContent(html)
          .openOn(map);
          }",
          gsub("`","\\\\`", as.character(stats_html))
        )
      )
    )
  ) %>% 
  hideGroup("Tourist Hotspots") %>% 
  addLegend(
    "bottomright",
    pal = pal, values = ~member_casual,
    title = "Customer Type"
  )



map
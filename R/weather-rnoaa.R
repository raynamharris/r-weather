library(rnoaa)
library(tidyverse)
library(lubridate)

stations <- ghcnd_stations() %>%
  as_tibble()
head(stations)

squaw_stations <- stations %>%
  filter(grepl('Squaw|SQUAW', name)) %>%
  filter(element == "SNWD", state == "CA", elevation > 1000) %>%
  pull(id)
head(squaw_stations)

snowy_stations <- stations %>%
  filter(grepl('SODA', name)) %>%
  filter(element == "SNOW", state == "CA", elevation > 1000)  %>%
  pull(id)
head(snowy_stations)


squaw_data <- meteo_pull_monitors(
    monitors = squaw_stations,
    keep_flags = FALSE,
    date_min = NULL,
    date_max = NULL,
    var = "all") %>%
  mutate(month = month(date),
        day = day(date),
        year = year(date)) %>%
  mutate(snwd2 = snwd / 304.8)
tail(squaw_data)


snowy_data <- meteo_pull_monitors(
  monitors = snowy_stations,
  keep_flags = FALSE,
  date_min = NULL,
  date_max = NULL,
  var = "all") %>%
  mutate(month = month(date),
         day = day(date),
         year = year(date)) %>%
  mutate(snow2 = snow / 304.8)
head(snowy_data)
  

squaw_data %>%
  filter(year > 2015) %>%
  ggplot(aes(x = date, y = snwd2)) +
  geom_line() +
  geom_hline(yintercept=1930/304.8) +
  theme_linedraw(base_size = 12) +
  labs(y = "Snow depth (ft)", x = NULL,
       caption = "Data collected from the Squaw Valley NOAA via `rnoaa`") +
  scale_x_date(date_breaks = "3 month", date_labels = "%b %d, %y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

squaw_data %>%
  #filter(year > 2015) %>%
  ggplot(aes(x = date, y = snwd2)) +
  geom_line() +
  geom_hline(yintercept=1930/304.8) +
  theme_linedraw(base_size = 12) +
  labs(y = "Snow depth (ft)", x = NULL,
       caption = "Data collected from the Squaw Valley NOAA via `rnoaa`") +
  scale_x_date(date_breaks = "1 year", date_labels = "%b %d, %y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


squaw_data %>%
  filter(month == 12,
         year <= 1974 | year >= 2002) %>%
  ggplot(aes(x = date, y = snwd2)) +
  geom_line() +
  geom_hline(yintercept=1930/304.8, color = "blue") +
  theme_linedraw(base_size = 12) +
  labs(y = "Snow depth (ft)", x = NULL,
       caption = "Data collected from the Squaw Valley NOAA via `rnoaa`") +
  scale_x_date(date_breaks = "1 week", date_labels = "%m/%d") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~year, scales = "free_x")

squaw_data %>%
  filter(month == 12,
         year <= 1974 | year >= 2002) %>%
  group_by(year) %>%
  summarise(snow_total = sum(snow) ) %>%
  ggplot(aes(x = year, y = snow_total)) +
  geom_bar(stat = "identity") +
  theme_linedraw(base_size = 12) +
  labs(y = "Snow depth (ft)", x = NULL,
       caption = "Data collected from the Squaw Valley NOAA via `rnoaa`")

snowy_data %>%
  filter(month == 12) %>%
  group_by(year, month, id) %>%
  summarise("Total snowfall in December" = sum(snow) / 304.8,
            "Max snow depth in December" = max(snwd) /304.8) %>%
  drop_na() %>%
  pivot_longer(cols = "Total snowfall in December":"Max snow depth in December",
               names_to = "variable", values_to = "value") %>%
  ggplot(aes(x = as.factor(year), y = value)) +
  geom_bar(stat = "identity", fill = "#88abff") +
  theme_linedraw(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~factor(variable,levels = c("Total snowfall in December", 
                                        "Max snow depth in December")), 
             nrow = 2, scales = "free_y") +
  labs(y = "Depth (ft)", x = "Years (when data was collected)", subtitle = "Data from Soda Springs, CA weather station via NOAA") +
  geom_text(aes(label = round(value,1)), 
            size = 3, vjust = 1)
  



library(rnoaa)
library(tidyverse)
library(lubridate)
library(cowplot)

library(devtools)

#remotes::install_github("ropensci/weathercan")
library(weathercan)

noaastations <- ghcnd_stations() %>%
  as_tibble()
head(noaastations)

mi_stations <- noaastations %>%
  filter(grepl('Lansing|LANSING', name)) %>%
  filter(element == "TMIN", state == "MI") %>%
  pull(id)
head(mi_stations)


mi_data <- meteo_pull_monitors(
  monitors = mi_stations,
  keep_flags = FALSE,
  date_min = NULL,
  date_max = NULL,
  var = "all") %>%
  mutate(date2 = date,
         month = month(date2),
         day = day(date2),
         year = year(date2))
tail(mi_data$tmin)
head(mi_data$tmin)


ca_stations <- noaastations %>%
  filter(grepl('Squaw|SQUAW', name)) %>%
  filter(element == "TMIN", state == "CA") %>%
  pull(id)
head(ca_stations)


ca_data <- meteo_pull_monitors(
  monitors = ca_stations,
  keep_flags = FALSE,
  date_min = NULL,
  date_max = NULL,
  var = "all") %>%
  mutate(date2 = date,
         month = month(date2),
         day = day(date2),
         year = year(date2))
tail(ca_data$tmin)
head(ca_data$tmin)



c <- ca_data %>%
  filter( year > 2008,
          id == "USR0000CSQU") %>%
  ggplot(aes(x = date, y = tmin,
             fill = tmin)) +
  geom_bar(stat = "identity") +
  labs(title = "Minimum air temp (C) ??????? in Olympic Valley, CA ",
       subtitle = "Data from station USR0000CSQU via `rnoaa`") +
  theme_linedraw(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%b %Y") 




a <- mi_data %>%
  filter( year > 2008,
         id == "USW00014836") %>%
  ggplot(aes(x = date, y = tmin,
             fill = tmin)) +
  geom_bar(stat = "identity") +
  labs(title = "Minimum air temp (C) ??????? in Lansing, MI ",
       subtitle = "Data from station USW00014836 via `rnoaa`") +
  theme_linedraw(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45,
                                 hjust = 1)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%b %Y") 
 


## weathercan

head(stations())


sasl <- stations_search("Saskatoon", interval = "day")
sasl

sasd <- weather_dl(station_ids = 47707, start = "2008-01-01", end = "2022-01-10")
head(sasd)


b <- sasd %>%
  group_by(month, day, year) %>%
  summarize(tmin = min(temp, na.rm = T)) %>%
  ungroup() %>%
  mutate(date = make_date(year, month, day)) %>%
  ggplot(aes(x = date, y = tmin, fill = tmin)) +
  geom_bar(stat = "identity") +
  theme_linedraw(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1)) +
  labs(title = "Daily minimum temp (C) in Saskatoon",
       subtitle = "Data from station 47707 via `weathercan`") +
  scale_x_date(date_breaks = "1 year", date_labels = "%b %Y") 

p <- plot_grid(a,c, b, nrow = 3)
p



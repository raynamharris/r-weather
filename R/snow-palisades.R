library(tidyverse)
library(lubridate)

# https://www.palisadestahoe.com/mountain-information/snowfall-tracker

df <- read_csv("../data/snow-2023-05-07.csv")

head(df)
names(df)

df2 <- df %>%
  mutate(Month = month(DATE), Year = year(DATE)) %>%
  mutate(samemonth = ymd(paste(Year, Month, "01", sep = "-"))) %>% 
  group_by(samemonth) %>%
  filter(SUMMITTOTAL == max(SUMMITTOTAL)) 
head(df2)

ggplot(df2, aes(x = DATE, y = SUMMITTOTAL) ) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = SUMMITTOTAL), vjust = -0.5,  size = 3) +
  theme_classic()

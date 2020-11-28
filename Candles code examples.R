#### SETUP ####
library(readxl)
library(tidyverse)
library(dplyr)
library(ggplot2)

Scented_All <- read_excel("Scented_all.xlsx") # Note that this dataset has data on 5 candles
Unscented_All <- read_excel("Unscented_all.xlsx")

#### SCENTED CANDLES ####
s <- Scented_All %>%
  arrange(Date) %>%
  filter(Date >= "2017-01-01") %>%
  filter(CandleID <= 3) %>%
  group_by(Date) %>%
  summarise(Rating=mean(Rating))

s1720 <- ggplot(s, aes(x = (as.Date(Date)), y = Rating)) +
  geom_vline(xintercept = as.numeric(as.Date("2020-01-20")), colour = "indianred1", linetype = "dashed")+
  geom_smooth(method = "loess", size = 1.5, colour = "lightseagreen", fill = "lightseagreen") +
  geom_point(alpha = 0.2, colour = "lightseagreen")+
  labs(x = "Date", y = "Average daily rating (1-5)", title = "Top 3 scented candles Amazon reviews 2017-2020")+
  theme_light()+
  theme(plot.title = element_text(size=16))+
  scale_x_date(date_labels = "%m-%Y", date_breaks = "6 month")


#### UNSCENTED CANDLES ####

us <- Unscented_All %>%
  arrange(Date) %>%
  filter(Date >= "2017-01-01") %>%
  group_by(Date) %>%
  summarise(Rating = mean(Rating))

us1720 <- ggplot(us, aes(x = (as.Date(Date)), y = Rating)) +
  geom_vline(xintercept = as.numeric(as.Date("2020-01-20")), colour = "indianred1", linetype = "dashed")+
  geom_smooth(method = "loess", size = 1.5, colour = "goldenrod3", fill = "goldenrod3") +
  geom_point(alpha = 0.2, colour = "goldenrod3")+
  labs(x = "Date", y = "Average daily rating (1-5)", title = "Top 3 unscented candles Amazon reviews 2017-2020")+
  theme_light()+
  theme(plot.title = element_text(size=16))+
  scale_x_date(date_labels = "%m-%Y", date_breaks = "6 month")


#### NO SCENT FUNCTION ####
no_scent <- function(x){
  case_when(
    str_detect(x, "[Nn]o scent") ~ "1", 
    str_detect(x, "[Nn]o smell") ~ "1",
    str_detect(x, "[Dd]oes not smell like") ~ "1",
    str_detect(x, "[Dd]oesn't smell like") ~ "1",
    str_detect(x, "[Cc]an't smell") ~ "1",
    str_detect(x, "[Cc]annot smell") ~ "1",
    str_detect(x, "[Ff]aint smell") ~ "1",
    str_detect(x, "[Ff]aint scent") ~ "1",
    str_detect(x, "[Dd]on't smell") ~ "1",
    str_detect(x, "[Ll]ike nothing") ~ "1",
    TRUE ~ x
  )
  
}

#### REVIEWS MENTIONING LACK OF SCENT 2020 ####
s5 <- Scented_All %>%
  arrange(Date) %>%
  filter(Date >= "2020-01-01") %>%
  mutate(noscent = no_scent(Review)) %>%
  mutate(noscent = ifelse(noscent != 1, 0, 1)) %>%
  mutate(month = reorder(format(Date, '%B'), Date)) %>%
  group_by(month) %>%
  add_tally() %>%
  mutate(se = (sd(noscent))/sqrt(n)) %>%
  summarise(n =n, se=se, noscent = sum(noscent)) %>%
  mutate(nsprop = noscent/n) %>%
  summarise(n=mean(n), se=mean(se), nsprop=mean(nsprop))

s5r <- ggplot(s5, aes(x=as.factor(month), y = nsprop, group = month))+
  geom_bar(stat = "identity", fill = "lightseagreen")+
  geom_errorbar(aes(ymin = (nsprop-se), ymax = (nsprop+se)), width=0.2, colour = "gray30")+
  labs(x = "Month", y = "Proportion of reviews", title = "Top 5 scented candles on Amazon: \nProportion of reviews mentioning lack of scent by month 2020")+
  theme_light()+
  theme(plot.title = element_text(size=16))
s5r


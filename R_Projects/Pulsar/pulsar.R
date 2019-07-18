
#To recreate the Joy Division cover for their Unknown Pleasures album:
#1. Read the csv
#2. Plot with the packages

#Optionally animate it as per this bloke's post
# https://www.r-bloggers.com/recreating-unknown-pleasures-graphic/

#More about the 'Little Green Men' here
# https://en.wikipedia.org/wiki/PSR_B1919%2B21


#install.packages("ggridges")
#install.packages("gganimate")
#install.packages("gifski")
#install.packages("png")

library(dplyr)
library(tidyr)

library(ggplot2)
library(ggridges)

library(gganimate)
library(gifski)
library(png)

# Read the CSV data
#~/R/pulsar_data/pulsar.csv

CSV_URL = "~/R/pulsar_data/pulsar.csv"

pulsar <- read.csv(CSV_URL, header = FALSE) %>%
  mutate(row = row_number()) %>%
  gather(col, height, -row) %>%
  mutate(
    col = sub("^V", "", col) %>% as.integer()
  )

# Plot baby plot
ggplot(pulsar, aes(x = col, y = row, height = height, group = row)) +
  geom_ridgeline(min_height = min(pulsar$height),
                 scale= 0.2,
                 size = 1,
                 fill = "black",
                 colour = "white") +
  scale_y_reverse() +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "black"),
    plot.background = element_rect(fill = "black", color = "black")
  )

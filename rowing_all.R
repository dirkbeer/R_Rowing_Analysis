rm(list = ls())
graphics.off()

library(ggplot2)
library(dplyr)
library(tidyverse)
library(purrr) # for map() and reduce()

# Define a directory (on Windows, use double backslashes, on Linux and iOS use single forward slashes)
directory <- "Z:\\Drive\\Drive\\Health\\rowerg_data"

# Find all CSV files in the directory
csv_files <- list.files(directory, pattern = "\\.csv$", full.names = TRUE)

# Process both data frames similarly
process_data <- function(file) {
  dat <- read.csv(file)
  
  # Add identifier variables
  dat$Source <- stringr::str_extract(file, "\\d+(?=\\.csv)")
  
  dat$Time..minutes. <- dat$Time..seconds. / 60
  
  dat <- dat %>%
    arrange(Time..seconds.) %>%
    mutate(Heart.Rate.Old = lag(Heart.Rate, 7), Joules.per.Beat = Watts / Heart.Rate.Old) %>%
    select(-Heart.Rate.Old)
  
  dat <- na.omit(dat)
  
  return(dat)
}

# Process all CSV files
data_list <- map(csv_files, process_data)

# Combine data
dat <- reduce(data_list, rbind)

# Define a function to convert seconds to mm:ss format
convert_to_mm_ss <- function(seconds) {
  seconds <- round(abs(seconds)) # so that negative seconds can by used to reverse the pace axis
  minutes <- seconds %/% 60
  seconds <- seconds %% 60
  sprintf("%02d:%02d", minutes, seconds)
}

p <- ggplot(dat, aes(x = Time..minutes., y = Joules.per.Beat, color = Source)) +
  scale_x_continuous(labels = convert_to_mm_ss) +
  geom_point()

print(p)

p1 <- ggplot(dat, aes(x = Heart.Rate, y = Watts, color = Source)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

print(p1)

p2 <- ggplot(dat, aes(x = Heart.Rate, y = -Pace..seconds., color = Source)) +
  scale_y_continuous(labels = convert_to_mm_ss) +
  geom_point() +
  labs(y = "Pace..seconds.") 

print(p2)

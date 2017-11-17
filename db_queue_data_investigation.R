library(tidyverse)

################### Data investigation ################
df <- read_rds("../saved_data_frames/db_queue_data.rds")

df %>%
  group_by(number, date, name) %>%
  summarise(
    cust_ia = mean(cust_ia, na.rm = TRUE),
    service_ia = mean(service_ia, na.rm = TRUE)
  ) %>%
  
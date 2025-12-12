#--Tim took drone images and extracted coverage data
#--He calls the north plots a and south plots b

library(readxl)
library(tidyverse)
library(lubridate)



# 1. raw data -------------------------------------------------------------

d1 <-
  read_excel("data-raw/drone/2024_dronedata/coverage/2025-02-07_tim-drone-coverage.xlsx") %>% 
  janitor::clean_names() %>% 
  mutate(date = ymd("2025-02-07"))


# 2. fix some details -----------------------------------------------------

d2 <- 
  d1 %>% 
  mutate(plot_idtim = case_when(
    roi_names_feature_names == '101ab' ~ '101b',
    TRUE ~ roi_names_feature_names
  ))
  


# 3. clean up -------------------------------------------------------------

d3 <-
  d2 %>% 
  mutate(trial_id = "Trial1") %>% 
  select(trial_id, date, plot_idtim, everything(), -roi_names_feature_names)



# write it ----------------------------------------------------------------


sexy1_coverage <- d3
  
usethis::use_data(sexy1_coverage, overwrite = TRUE)

sexy1_eukey %>% 
  write_csv("inst/extdata/sexy1_coverage.csv")

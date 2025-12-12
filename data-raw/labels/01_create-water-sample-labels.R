#--use eukey and trtkey to create labels for water sampling
#--water samples pull from entire plot (ex. 101N and 101S)

library(readxl)
library(tidyverse)


# 1. raw data -------------------------------------------------------------

d1 <-
  read_excel("data-raw/eukey/sexy1_eukey.xlsx", skip = 5) %>% 
  fill(trt_id) %>% 
  mutate(block = paste0("b", block), #--to make sure it is never numeric
         plot_id = paste0(block, "_", plot, "_", plothalf, "_", samptype),
         trial = "Sexy1") %>% 
  select(trial, eu_id, plot_id, trt_id)

#--first sampling date (date = 001)
d2 <- 
  d1 %>% 
  separate(plot_id, sep = "_", into = c("block", "plot", "half", "x")) %>% 
  select(trial, block, plot, trt_id) %>% 
  distinct() %>% 
  mutate(
    trial_nu = sprintf("%03d", 1),
    date = sprintf("%03d", 1),
    n = 1:n(),
    n2 = sprintf("%03d", n),
    watersample_id = paste0(trial_nu, date, n2)) %>% 
  select(trial, block, plot, trt_id, watersample_id)

d2

# store it for user -------------------------------------------------------

sexy1_wskey001 <- d2

sexy1_wskey001 %>% 
  write_csv("data-raw/labels/sexy1_wskey001.csv")

# make internal data ------------------------------------------------------

# internal_eukey1 <- sexy1_eukey
# 
# usethis::use_data(internal_eukey1, internal = TRUE, overwrite = TRUE)

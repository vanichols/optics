#########################
# created: 8 Nov 2024
#
# last updated: 19 march 2025 (use new eukey style)
#                26 march 2025 (added spring counts)
#
# purpose: Process standcount data
#
# NOTES: Some plots had annual rye planted later (field mistake), may need to update
#        Spring counts - only did two samples per half plot
#
#########################


##### Clear environment and load packages #####
rm(list=ls())
library(tidyverse)
library(lubridate)
library(readxl) #--used to read Excel files
library(janitor) #--used to clean data


# 2024 Fall --------------------------------------------------------------------

eukey <- read_csv("inst/extdata/sexy1_eukey.csv")
plotkey <- read_csv("inst/extdata/sexy1_plotkey.csv")

k <- 
  eukey %>% 
  left_join(plotkey)

scraw1 <- read_excel("data-raw/standcounts/rd_20241108_standcounts.xlsx", skip = 5, na = "NA")
scraw2 <- read_excel("data-raw/standcounts/rd_20250326_standcounts.xlsx", skip = 5, na = "NA")

# F. process fall counts -----------------------------------------------------------------

#--clean up date and pivot
f1 <-
  scraw1 %>%
  fill(date) %>% 
  select(-notes) %>% 
  pivot_longer(s1:s4, names_to = "subsample", values_to = "temp_plants_m2") 

#--sum up subsamples to get 8 m of counting
f2 <- 
  f1 %>% 
  #--8 samples sums up to 8 m of linear sampling (only did 4 per plot half), 
  #--with 12.5 cm row widths this equates to plants/m2
  group_by(date, plot) %>% 
  summarise(plants_m2 = sum(temp_plants_m2, na.rm = T))

#--address issues
#--plots 201, 202 both had half a lane patchy that Gina avoided, which was the wrong thing to do
#--plot 203 missing half its plants (annual rows)
#--plot 212 missing half its plants (annual rows)
#--plot 309 missing one half completely

f3 <- 
  f2 %>% 
  mutate(plants_m2 = case_when(
    plot == 201 ~ plants_m2 * 0.95,
    plot == 202 ~ plants_m2 * 0.95,
    plot == 203 ~ plants_m2 * 0.5,
    plot == 212 ~ plants_m2 * 0.5,
    plot == 309 ~ plants_m2 * 2,
    TRUE ~ plants_m2
  ))

#--rejoing to eu

f4 <- 
  f3 %>% 
  ungroup() %>% 
  left_join(k) %>% 
  select(trial, date, eu_id, plot_id, plants_m2) %>% 
  distinct() %>% 
  mutate(date = as_date(date))

#--check it
f4 %>% 
  ggplot(aes(eu_id, plants_m2)) + 
  geom_point()


# S. process spring counts -----------------------------------------------------------------

#--clean up date and pivot
s1 <-
  scraw2 %>%
  fill(date) %>% 
  select(-notes) %>% 
  pivot_longer(s1:s4, names_to = "subsample", values_to = "temp_plants_m2") 

#--sum up subsamples to get 8 m of counting
s2 <- 
  s1 %>% 
  #--8 samples sums up to 8 m of linear sampling, but I only sampled 4 m (only did 2 per plot half), 
  #--with 12.5 cm row widths this equates to plants/m2
  #--just duplicate the two samples
  group_by(date, plot) %>% 
  summarise(plants_m2 = sum(temp_plants_m2, na.rm = T)*2)

#--address issues
#--plot 203 missing half its plants (annual rows)
#--plot 212 missing half its plants (annual rows)
#--plot 309 missing one half completely

s3 <- 
  s2 %>% 
  mutate(plants_m2 = case_when(
    plot == 203 ~ plants_m2 * 0.5,
    plot == 212 ~ plants_m2 * 0.5,
    plot == 309 ~ plants_m2 * 2,
    TRUE ~ plants_m2
  ))

#--rejoing to eu

s4 <- 
  s3 %>% 
  ungroup() %>% 
  left_join(k) %>% 
  select(trial, date, eu_id, plot_id, plants_m2) %>% 
  distinct() %>% 
  mutate(date = as_date(date))

#--check it
s4 %>% 
  ggplot(aes(eu_id, plants_m2)) + 
  geom_point()



# C. combine them ---------------------------------------------------------

d <- 
  f4 %>% 
  bind_rows(s4)

#--check it
d %>% 
  ggplot(aes(eu_id, plants_m2)) + 
  geom_point(aes(color = as.factor(date)))


# write it ----------------------------------------------------------------

sexy1_standcounts <- d

usethis::use_data(sexy1_standcounts, overwrite = T)

sexy1_standcounts %>% 
    write_csv("inst/extdata/sexy1_standcounts.csv")

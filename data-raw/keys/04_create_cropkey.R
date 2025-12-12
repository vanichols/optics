#--manually created a cropkey

library(readxl)
library(tidyverse)

rm(list = ls())

# 1. raw data -------------------------------------------------------------

d1a <-
  read_csv("inst/extdata/op_trtkey.csv") %>%
  select(crop_id) %>%
  filter(crop_id %in% c("a", "p")) %>%
  distinct()

env_id <- c("sexy1-24/25", "sexy2-25/26")

d1 <-
  crossing(d1a, env_id) |>
  add_row(crop_id = "p",
          env_id = "eusun1-24/25") |>
  add_row(crop_id = "p",
          env_id = "seed1-24/25") |>
  add_row(crop_id = "p",
          env_id = "seed1-25/26")




# 2. what is it -----------------------------------------------------------

d2 <-
  d1 %>%
  mutate(cropdesc = case_when(
    env_id == "sexy1-24/25" & crop_id == "p" ~ "Perennial cereal rye population from Germany, seed from Foulum 23/24 season",
    env_id == "sexy1-24/25" & crop_id == "a" ~ "Annual cereal rye (SU Thor) hybrid",

    env_id == "sexy2-25/26" & crop_id == "p" ~ "Perennial cereal rye population from RePhilDK25 (seed increase in Flakkebjerg)",
    env_id == "sexy2-25/26" & crop_id == "a" ~ "Annual cereal rye (variety??) hybrid",

    env_id == "eusun1-24/25" ~ "Perennial cereal rye population from Germany, seed from Foulum 23/24 season",

    env_id == "seed1-24/25" ~ "Perennial cereal rye population from Germany, seed from Foulum 23/24 season",
    env_id == "seed1-25/26" ~ "Perennial cereal rye population from Germany, seed from RePhilDK25",

    TRUE ~ "XX"
  ))


# 3. tkw-----------------------------------------------------------

d3 <-
  d2 %>%
  mutate(tkw_of_planted_seed_g = case_when(
    env_id == "sexy1-24/25" & crop_id == "p" ~ "27",
    env_id == "eusun1-24/25" & crop_id == "p" ~ "27",

    env_id == "seed1-24/25" & crop_id == "p" ~ "27",
    env_id == "seed1-25/26" & crop_id == "p" ~ "23.5",

    env_id == "sexy1-24/25" & crop_id == "a" ~ "42",

    env_id == "sexy2-25/26" & crop_id == "p" ~ "23.5",
    env_id == "sexy2-25/26" & crop_id == "a" ~ "41.5",


    TRUE ~ "XX"
  ))


# 4. germination -----------------------------------------------------------

d4 <-
  d3 %>%
  mutate(germination_pct = case_when(
    env_id == "sexy1-24/25" & crop_id == "p" ~ "86",
    env_id == "eusun1-24/25" & crop_id == "p" ~ "86",
    env_id == "sexy1-24/25" & crop_id == "a" ~ "95",

    env_id == "seed1-24/25" & crop_id == "p" ~ "86",
    env_id == "seed1-25/26" & crop_id == "p" ~ "65",

    env_id == "sexy2-25/26" & crop_id == "p" ~ "65",
    env_id == "sexy2-25/26" & crop_id == "a" ~ "85",
    TRUE ~ "XX"
  ))


# 5. ergot infection -----------------------------------------------------------

d5 <-
  d4 %>%
  mutate(ergot_pct_when_received = case_when(
    env_id == "sexy1-24/25" & crop_id == "p" ~ "2",
    TRUE ~ NA
  ))


d5

# make data ---------------------------------------------------------------

op_cropkey <-
  d5 |>
  select(env_id, crop_id, everything())

usethis::use_data(op_cropkey, overwrite = TRUE)



# make available to user --------------------------------------------------

op_cropkey %>%
  write_csv("inst/extdata/op_cropkey.csv")



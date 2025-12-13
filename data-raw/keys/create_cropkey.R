#--manually created a cropkey

library(readxl)
library(tidyverse)

rm(list = ls())

# 1. raw data -------------------------------------------------------------

d1 <-
  read_csv("inst/extdata/op_trtkey.csv") |>
  select(env_key, crop_id) |>
  distinct()

# 2. provenance of seed -----------------------------------------------------------

d2 <-
  d1 %>%
  mutate(crop_desc = case_when(
    env_key == "0101" & crop_id == "p" ~ "Perennial cereal rye population from Germany, seed from Foulum 23/24 season (which had 2% ergot when received)",
    env_key == "0101" & crop_id == "a" ~ "Annual cereal rye (SU Thor) hybrid",

    env_key == "0202" & crop_id == "p" ~ "Perennial cereal rye population from RePhilDK25 (seed increase in Flakkebjerg)",
    env_key == "0202" & crop_id == "a" ~ "Annual cereal rye (variety??) hybrid",

    env_key == "0001" ~ "Perennial cereal rye population from Germany, seed from Foulum 23/24 season",

    env_key == "0301" ~ "Perennial cereal rye population from Germany, seed from Foulum 23/24 season",
    env_key == "0302" ~ "Perennial cereal rye population from Germany, seed from RePhilDK25",

    crop_id == "mix" ~ NA,
    TRUE ~ "XX"
  ))


# 3. tkw-----------------------------------------------------------

d3 <-
  d2 %>%
  mutate(tkw_of_planted_seed_g = case_when(
    env_key == "0101" & crop_id == "p" ~ "27",
    env_key == "0001" & crop_id == "p" ~ "27",

    env_key == "0301" & crop_id == "p" ~ "27",
    env_key == "0302" & crop_id == "p" ~ "23.5", #--seed inc source

    env_key == "0101" & crop_id == "a" ~ "42",

    env_key == "0202" & crop_id == "p" ~ "23.5",#--seed inc source
    env_key == "0202" & crop_id == "a" ~ "41.5",

    crop_id == "mix" ~ NA,
    TRUE ~ "XX"
  ))


# 4. germination -----------------------------------------------------------

d4 <-
  d3 %>%
  mutate(germination_pct = case_when(
    env_key == "0101" & crop_id == "p" ~ "86",
    env_key == "0001" & crop_id == "p" ~ "86",
    env_key == "0101" & crop_id == "a" ~ "95",

    env_key == "0301" & crop_id == "p" ~ "86",
    env_key == "0302" & crop_id == "p" ~ "65",

    env_key == "0202" & crop_id == "p" ~ "65",
    env_key == "0202" & crop_id == "a" ~ "85",
    crop_id == "mix" ~ NA,
    TRUE ~ "XX"
  ))


# make data ---------------------------------------------------------------

op_cropkey <-
  d4

usethis::use_data(op_cropkey, overwrite = TRUE)

op_cropkey %>%
  write_csv("inst/extdata/op_cropkey.csv")



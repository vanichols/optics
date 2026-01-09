#--manually created an experimental unit key
#--The plots are double wide, with the north plots for measurements and the south plots for yields
#--updated 12 dec 2025

library(readxl)
library(tidyverse)


# ###############sexy1-24/25 ----------------------------------------------

# 1. raw data -------------------------------------------------------------

a1 <-
  read_excel("data-raw/keys/sexy1-2024_eukey.xlsx", skip = 5) %>%
  fill(trt_id) %>%
  mutate(env_id = "sexy1-24/25") %>%
  select(env_id, plot_id = plot, trt_id)

# ###############sexy2-25/26 ----------------------------------------------

b1 <-
  read_excel("data-raw/keys/sexy2-2025_eukey.xlsx", skip = 5) %>%
  mutate(env_id = "sexy2-25/26") %>%
  select(env_id, plot_id = plot, trt_id = trt)

# ###############seed1-24/25 ----------------------------------------------

#--the seed increase field, no plots just one big spot

c1 <-
  tibble(env_id = rep("seed1-24/25", 4),
         plot_id = c(1,2, 3, 4),
         trt_id = rep("mech_pwide24", 4))


# ###############eusun1-24/25 ----------------------------------------------

d1 <-
  read_excel("data-raw/keys/Eusun-treatments-from-Casandra.xlsx", sheet = "Grain", skip = 2) |>
  janitor::clean_names() |>
  select(plot_id = plot, trt_id = treatment) |>  #--need to explain this somewhere else
  mutate(env_id = "eusun1-24/25")

# ###############seed1-25/26 ----------------------------------------------

#--the seed increase field, this time there are plots

e1 <-
  tibble(env_id = rep("seed1-25/26", 4),
         plot_id = c(1, 2, 3, 4),
         trt_id = rep("mech_pwide25", 4))

# make data ---------------------------------------------------------------

op_plotkey <-
  a1 |>
  bind_rows(b1) |>
  bind_rows(c1) |>
  bind_rows(d1) |>
  bind_rows(e1)

usethis::use_data(op_plotkey, overwrite = TRUE)


# store it for accessing within package? -------------------------------------------------------

op_plotkey %>%
  write_csv("inst/extdata/op_plotkey.csv")

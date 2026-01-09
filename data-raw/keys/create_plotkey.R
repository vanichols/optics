library(readxl)
library(tidyverse)


# ###############sexy1-24/25 ----------------------------------------------

# 1. raw data -------------------------------------------------------------

a1 <-
  read_excel("data-raw/keys/sexy1-2024_eukey.xlsx", skip = 5) %>%
  fill(trt_id) %>%
  mutate(env_key = "0101",
         trt_key = paste(env_key, trt_id, sep = "_")) %>%
  select(trt_key, plot_id = plot) |>
  distinct() |>
  mutate(block_id = paste0("b", str_sub(plot_id, 1, 1)))

# ###############sexy2-25/26 ----------------------------------------------

b1 <-
  read_excel("data-raw/keys/sexy2-2025_eukey.xlsx", skip = 5) %>%
  mutate(env_key = "0202",
         trt_key = paste(env_key, trt, sep = "_")) %>%
  select(trt_key, plot_id = plot) |>
  mutate(block_id = paste0("b", str_sub(plot_id, 1, 1)))


# ###############seed1-24/25 ----------------------------------------------

#--the seed increase field, no plots just one big spot with 7 passes

c1 <-
  tibble(plot_id = c(1,2, 3, 4, 5, 6, 7),
         trt_key = rep("0301_pmechwide24", 7),
         block_id = NA)

# ###############seed1-25/26 ----------------------------------------------

#--the seed increase field, tilled up every other one...don't remember how many plots

d1a <-
  tibble(
         plot_id = c(1, 3, 5),
         trt_key = rep("0302_pmechwide25", 3))

d1b <-
  tibble(
       plot_id = c(2, 4, 6, 8),
       trt_key = rep("0302_pmechwide24", 4))


d1 <-
  d1a |>
  bind_rows(d1b) |>
  mutate(block_id = NA)

# ###############eusun1-24/25 ----------------------------------------------

e1 <-
  read_excel("data-raw/keys/Eusun-treatments-from-Casandra.xlsx", sheet = "Grain", skip = 2) |>
  janitor::clean_names() |>
  select(plot_id = plot, treatment) |>
  mutate(
    env_key = "0001",
    trt_id = str_remove(treatment, "_"),
    trt_key = paste0(env_key, "_", trt_id)) |>
  select(trt_key, plot_id) |>
  #--based on the plot map he shared
  mutate(block_id = case_when(
    plot_id %in% c(8, 55) ~ "b4",
    plot_id %in% c(13, 69) ~ "b3",
    plot_id %in% c(4, 3) ~ "b2",
    plot_id %in% c(13, 69) ~ "b1",
    TRUE ~ "UHOH"
  ))



# make data ---------------------------------------------------------------

f1 <-
  a1 |>
  bind_rows(b1) |>
  bind_rows(c1) |>
  bind_rows(d1) |>
  bind_rows(e1)

op_plotkey <-
  f1 |>
  separate(trt_key, into = c("env_key", "trt_id"), remove = F) |>
  mutate(plot_key = paste0(env_key, "_", plot_id)) |>
  select(env_key, plot_key, trt_key, plot_id, block_id)


usethis::use_data(op_plotkey, overwrite = TRUE)


op_plotkey %>%
  write_csv("inst/extdata/op_plotkey.csv")

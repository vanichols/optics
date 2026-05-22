library(tidyverse)

#--for the 24/25 season, these sensor ids were assigned to these plots

load("data/op_eukey.rda")
load("data/op_trtkey.rda")
load("data/op_envkey.rda")
op_envkey

# sexy1 season 24/25 ------------------------------------------------------

#--only put sensors in p and a trts
a1 <-
  op_eukey |>
  filter(env_key == "0101") |> #--sexy1 season 24/25
  select(-eu_key) |>
  left_join(op_trtkey |> select(env_key, trt_key, trt_id)) |>
  select(-trt_key) |>
  filter(trt_id %in% c("p", "a"))

a2 <-
  a1 |>
  mutate(soilsensor_id = case_when(
  plot_id == "101" ~ "94243864",
  plot_id == "105" ~ "94243868",
  plot_id == "202" ~ "94243867",
  plot_id == "209" ~ "94243869",
  plot_id == "307" ~ "94243865",
  plot_id == "311" ~ "94243862",
  plot_id == "405" ~ "94243861",
  plot_id == "412" ~ "94243866",
  TRUE ~ "0000000"
  ))

a2

# sexy2 season 25/26 ------------------------------------------------------

op_envkey

b1 <-
  op_eukey |>
  filter(env_key == "0202") |> #--sexy1 season 24/25
  select(-eu_key) |>
  left_join(op_trtkey |> select(env_key, trt_key, trt_id)) |>
  select(-trt_key) |>
  filter(trt_id %in% c("p", "a"))


b2 <-
  b1 |>
  mutate(soilsensor_id = case_when(
    plot_id == "102" ~ "94243868",
    plot_id == "112" ~ "94243865",
    plot_id == "210" ~ "94243864",
    plot_id == "211" ~ "94243869",
    plot_id == "303" ~ "94243862",
    plot_id == "308" ~ "94243866",
    plot_id == "402" ~ "94243861",
    plot_id == "409" ~ "94243867",
    TRUE ~ "0000000"
  ))

b2



# put together ------------------------------------------------------------

data <-
  a2 |>
  bind_rows(b2)

op_soilsensorkey <- data

usethis::use_data(op_soilsensorkey, overwrite = T)

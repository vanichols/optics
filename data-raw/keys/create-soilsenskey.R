library(tidyverse)

#--for the 24/25 season, these sensor ids were assigned to these plots

load("data/op_plotkey.rda")
load("data/op_trtkey.rda")

#--only put sensors in p and a trts
d1 <-
  op_plotkey |>
  filter(env_key == "0101") |> #--sexy1 season 24/25
  select(-plot_key) |>
  left_join(op_trtkey |> select(env_key, trt_key, trt_id)) |>
  select(-trt_key) |>
  filter(trt_id %in% c("p", "a"))

d2 <-
  d1 |>
  mutate(soilsens_id = case_when(
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

d2


op_soilsenskey <- d2

usethis::use_data(op_soilsenskey, overwrite = T)

sexy1_soilmois %>%
  write_csv("inst/extdata/op_soilsenskey.csv")

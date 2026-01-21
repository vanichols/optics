library(tidyverse)

load("data-raw/emma/cc_biomass.rda")

cc_biomass |>
  filter(sampledate_id == "t2") |>
  group_by(trt_id) |>
  summarise(samplearea_m2 = mean(samplearea_m2),
            wholeplant_g = mean(wholeplant_g)) |>
  mutate(bio_kgha = wholeplant_g/samplearea_m2 * 10000 / 1000)

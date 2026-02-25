#--cecilie's data taken from the o-drive 25 feb 2026
#--it only has one location, it is missing eusun1, emailed her

library(tidyverse)
library(readxl)

d <-
  read_excel("data-raw/ergot/2025 harvest ergot weight flakkebjerg.xlsx") |>
  janitor::clean_names()


d1 <-
  d |>
  separate(sample_id, into = c("env_key", "plot_id", "sampletype_id"), sep = "-") |>
  filter(sampletype_id != "A") |>
  select(-sample_desc) |>
  pivot_wider(names_from = sampletype_id, values_from = weight_in_g) |>
  mutate(pct_ergot_by_weight = B1/(B1+B2)*100)


d1 |>
  ggplot(aes(loc_id, pct_ergot_by_weight)) +
  geom_point()

#--get treatments for figure
d1 |>
  left_join(op_plotkey) |>
  left_join(op_trtkey) |>
  ggplot(aes(crop_id, pct_ergot_by_weight)) +
  geom_point() +
  facet_wrap(~loc_id)

#--can we look at it as a percentage of kernals? using tkw_?


#--so weight of ergot per 1000 kernals is about the same
#--there are more kernals per unit weight, so as a pct of kernals, ergot will result in a raw value being higher
d1 |>
  left_join(op_plotkey) |>
  left_join(op_trtkey) |>
  left_join(
    op_yields |>
      filter(name == "tkw_g")
  ) |>
  mutate(B2_kernals = B2 / value) |>
  select(B2_kernals, everything()) |>
  mutate(grams_ergot_per_1000kernals = B1/B2_kernals) |>
  ggplot(aes(crop_id, grams_ergot_per_1000kernals)) +
  geom_point() +
  facet_wrap(~loc_id)


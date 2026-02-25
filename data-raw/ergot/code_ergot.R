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

#--get treatments
d1 |>
  left_join(op_plotkey) |>
  left_join(op_trtkey) |>
  select()

op_plotkey

library(tidyverse)
library(readxl)

#--put into standard response variable format
#--name (ergothioniene_ugg)
#--value (numeric)
#--units (micrograms per gram)

load("data/op_eukey.rda")
load("data/op_envkey.rda")

op_envkey

# data --------------------------------------------------------------------

draw <- read_excel("data-raw/ergothioniene/2026.04.22_rye_gina 2026 from Ben.xlsx",
           sheet = "gina  final",
           skip = 1) |>
  janitor::clean_names()


draw |>
  ggplot(aes(trt_id, mg_g)) +
  geom_jitter(width = 0.1, aes(color = as.factor(plot_id)))

draw |>
  ggplot(aes(plot_id, mg_g)) +
  geom_jitter(width = 0.1, aes(color = as.factor(plot_id))) +
  facet_grid(.~trt_id)

draw


# make based on eu --------------------------------------------------------

d1 <-
  draw |>
  mutate(plot_id = as.character(plot_id)) |>
  left_join(op_envkey |>
              filter(sea_id == "24/25"),
            relationship = 'many-to-many') |> #--bc we have plot halfs
  left_join(op_eukey)

#--keep the standard info
op_ergo <-
  d1 |>
  mutate(units = "micrograms per gram",
               name = "ergothioniene_ugg") |>
  select(eu_key, loc_id, sea_id, plot_id, trt_id, plot_half, name, value = mg_g, units)


usethis::use_data(op_ergo, overwrite = TRUE)


op_ergo %>%
  write_csv("inst/extdata/op_ergo.csv")

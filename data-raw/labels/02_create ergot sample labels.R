Sys.setenv(LANGUAGE = "en")

# ergot samples from 24/25 growing season ---------------------------------
rm(list = ls())

load("data/op_envkey.rda")

d1 <-
  op_envkey |>
  filter(env_key %in% c("0001", "0101")) |>
  select(env_key, loc_id, sea_id)

load("data/op_plotkey.rda")

d2 <-
  d1 |>
  left_join(op_plotkey |> select(env_key, plot_id))

#--create sample type key

sample_type <- tibble(sample_type = c("A", "B1", "B2"),
                      sample_desc = c("raw", "sep-ergot", "sep-grain"))

d3 <-
  d2 |>
  crossing(sample_type)

#--Excel is annoyed by the leading zeros in the environmental key, add an 'E'

d4 <-
  d3 |>
  mutate(env_key = paste0("E-", env_key))

write.table(
  d4,
  file = "data-raw/labels/ergot-season01-labels.csv",
  sep = ";",
  dec = ",",
  row.names = FALSE
)

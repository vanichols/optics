

library(readxl)
library(tidyverse)


# location key ------------------------------------------------------------

# loc_key
# loc_id
# loc_au
# loc_fieldnum

#--add a new row when I add a location

a1 <-
  tribble(
  ~loc_key,  ~ loc_id,    ~loc_au,      ~loc_fieldnum,
  "00",         "eusun1",   "foulum",     "unknown",
  "01",         "sexy1",   "flakkebjerg",  "22",
  "02",         "sexy2",   "flakkebjerg",  "06",
  "03",         "seed1",   "flakkebjerg",  "19",
  "04",         "cry1",   "flakkebjerg",   "TBD")

op_lockey <-
  a1


# growing season key ------------------------------------------------------------

# sea_key
# sea_id
# sea_description

#--add a new row when adding new season

b1 <-
  tribble(
    ~sea_key,  ~ sea_id,    ~sea_desc,
    "01",         "24/25",   "fall 2024 through end of 2025",
    "02",         "25/26",   "fall 2025 through end of 2026")

op_seakey <-
  b1


# combine -----------------------------------------------------------------

op_envkey <-
  crossing(op_seakey |> select(sea_key),
         op_lockey |> select(loc_key)) |>
  unite(loc_key, sea_key, col = "env_key", sep = "", remove = F) |>
  left_join(op_seakey) |>
  left_join(op_lockey)

usethis::use_data(op_envkey, overwrite = TRUE)

op_seakey %>%
  write_csv("inst/extdata/op_envkey.csv")

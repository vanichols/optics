#--manually created a cctrtkey
#--need to know cover crop planting details...
#--haven't fixed yet to match other keys...

library(readxl)
library(tidyverse)


# 1. raw data -------------------------------------------------------------

d1 <-
  read_csv("inst/extdata/op_trtkey.csv") |>
  select(cctrt_id)  |>
  distinct() |>
  mutate(env_id = c("sexy1-24/25"))


# 2. description ------------------------------------------------------

d2 <-
  d1 |>
  mutate(desc = c("No cover crop planted", "Fall cover crop planted after grain harvest"))

# 3. ccplanting date ------------------------------------------------------

#--etc....to be added once it is done
#--depth, rate, method (broadcast?), variety, etc.

d3 <-
  d2 |>
  #--date of planting
  mutate(dop_dmy = c(NA, "19-08-2025"),
         dop_ymd = dmy(dop_dmy)) |>
  #--what was planted
  mutate(cc_details = c(NA, "10.5 kg mixture of 90% oil seed radish and 10% phacelia (by weight) seeded in X cm rows with a disc drill, annual rye plots had 2 passes of light tillage 2-5 cm depth"))


# make data ---------------------------------------------------------------

op_cctrtkey <-
  d3 |>
  select(env_id, cctrt_id, everything())

usethis::use_data(op_cctrtkey, overwrite = TRUE)

# make available ----------------------------------------------------------

op_cctrtkey |>
  write_csv("inst/extdata/op_cctrtkey.csv")

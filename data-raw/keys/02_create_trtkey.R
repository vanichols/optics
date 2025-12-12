#--manually created a trtkey
#--NEED TO ADD EUSUN's stuff!

library(readxl)
library(tidyverse)

rm(list = ls())

# 1. raw data -------------------------------------------------------------

#--sexy1
d1a <-
  read_excel("data-raw/keys/sexy1-2024_eukey.xlsx", skip = 5) |>
  select(trt_id) |>
  filter(!is.na(trt_id)) |>
  distinct() |>
  mutate(env_id = "sexy1-24/25",
         pdate_ymd = ymd("2024-10-18"),
         hdate_ymd = ymd("2025-08-08"))

#--seed inc1
d1b <-
  read_csv("inst/extdata/op_plotkey.csv") |>
  filter(env_id == "seed1-24/25") |>
  select(env_id, trt_id) |>
  distinct() |>
  mutate(pdate_ymd = ymd("2024-10-18"),
         hdate_ymd = ymd("2025-08-15"))

#--sexy2
d1c <-
  read_excel("data-raw/keys/sexy2-2025_eukey.xlsx", skip = 5) |>
  select(trt_id = trt) |>
  distinct() |>
  mutate(env_id = "sexy2-25/26",
         pdate_ymd = ymd("2025-09-26"),
         hdate_ymd = NA)

#--eusun1
d1d <-
  read_csv("inst/extdata/op_plotkey.csv") |>
  filter(env_id == "eusun1-24/25") |>
  select(env_id, trt_id) |>
  distinct() |>
  mutate(pdate_ymd = ymd("2024-12-31"), #--need to ask casandra
         hdate_ymd = ymd("2025-08-19"))

#--seed inc2
d1b <-
  read_csv("inst/extdata/op_plotkey.csv") |>
  filter(env_id == "seed1-25/26") |>
  select(env_id, trt_id) |>
  distinct() |>
  mutate(pdate_ymd = ymd("2025-09-25"),
         hdate_ymd = NA)



d1 <-
  d1a |>
  bind_rows(d1b) |>
  bind_rows(d1c) |>
  bind_rows(d1d)


# 2. add trt_desc -------------------------------------------------------

d2 <-
  d1 |>
  mutate(trt_desc = case_when(
    trt_id == "p" ~ "Perennial cereal (PC) rye in 12.5 cm rows",
    trt_id == "xp" ~ "PC rye in 12.5 cm rows without weed control",
    trt_id == "pcc" ~ "PC rye in 12.5 cm rows with a cover crop planted after grain harvest",
    trt_id == "xpcc" ~ "PC rye in 12.5 cm rows with a cover crop planted after grain harvest without weed control",

    trt_id == "a" ~ "Annual cereal rye hybrid (A) planted in 12.5 cm rows",
    trt_id == "xa" ~ "A planted in 12.5 cm rows without weed control",
    trt_id == "acc" ~ "A planted in 12.5 cm rows with a cover crop planted after grain harvest",
    trt_id == "xacc" ~ "A planted in 12.5 cm rows with a cover crop planted after grain harvest without weed control",

    trt_id == "aprows" ~ "P and A planted in alternating 12.5 cm rows",
    trt_id == "xaprows" ~ "P and A planted in alternating 12.5 cm rows without weed control",
    trt_id == "apmix" ~ "P and A mixture planted in 12.5 cm rows",
    trt_id == "xapmix" ~ "P and A mixture planted in 12.5 cm rows without weed control",

    trt_id == "mech_pwide24" ~ "PC rye planted in 25 cm rows with mechanical weed control (for a seed increase) in 2024",
    trt_id == "mech_pwide25" ~ "PC rye planted in 25 cm rows with mechanical weed control inside seed1-24/25 in 2025, created plots of different stand ages",
    trt_id == "17_West" ~ "PC rye planted in XX cm rows at 1/3 plant density (100 pl m-2), unknown biomass harvest and row width", #---emailed casandra
    trt_id == "18_West" ~ "PC rye planted in XX cm rows at 1/3 plant density (100 pl m-2), unknown biomass harvest and row width",
    trt_id == "17_East" ~ "PC rye planted in XX cm rows at full plant density (300 pl m-2), unknown biomass harvest and row width",
    trt_id == "18_East" ~ "PC rye planted in XX cm rows at full plant density (300 pl m-2), unknown biomass harvest and row width"
  ))

# 3. add crop_id -------------------------------------------------------

#--make a crop_id (a, p, mix)

d3 <-
  d2 |>
  mutate(crop_id = case_when(
    trt_id == "p" ~ "p",
    trt_id == "xp" ~ "p",
    trt_id == "pcc" ~ "p",
    trt_id == "xpcc" ~ "p",

    trt_id == "a" ~ "a",
    trt_id == "xa" ~ "a",
    trt_id == "acc" ~ "a",
    trt_id == "xacc" ~ "a",

    trt_id == "aprows" ~ "mix",
    trt_id == "xaprows" ~ "mix",
    trt_id == "apmix" ~ "mix",
    trt_id == "xapmix" ~ "mix",

    trt_id == "mech_pwide24" ~ "p",
    trt_id == "mech_pwide25" ~ "p",
    trt_id == "17_West" ~ "p",
    trt_id == "18_West" ~ "p",
    trt_id == "17_East" ~ "p",
    trt_id == "18_East" ~ "p"
  ))



# 4. make a cctrt_id ------------------------------------------------------

#--make a cctrt_id (nocc, fcc)

d4 <-
  d3 |>
  mutate(cctrt_id = case_when(
    env_id == "sexy1-24/25" & trt_id == "p" ~ "nocc",
    env_id == "sexy2-25/26" & trt_id == "p" ~ "nocc",
    trt_id == "xp" ~ "nocc",
    trt_id == "pcc" ~ "fcc",
    trt_id == "xpcc" ~ "fcc",

    trt_id == "a" ~ "nocc",
    trt_id == "xa" ~ "nocc",
    trt_id == "acc" ~ "fcc",
    trt_id == "xacc" ~ "fcc",

    trt_id == "aprows" ~ "nocc",
    trt_id == "xaprows" ~ "nocc",
    trt_id == "apmix" ~ "nocc",
    trt_id == "xapmix" ~ "nocc",

    TRUE ~ NA

  ))



# 5. planting rate -----------------------------------------------------------

d5 <-
  d4 %>%
  mutate(plantingrate_kgha = case_when(

    env_id == "sexy1-24/25" & crop_id == "p" ~ "94",
    env_id == "sexy1-24/25" & crop_id == "a" ~ "132",
    env_id == "sexy1-24/25" & crop_id == "mix" ~ "113",
    env_id == "seed1-24/25" & crop_id == "p" ~ "94",

    #--lower planting rate bc was planted earlier
    env_id == "sexy2-25/26" & crop_id == "p" ~ "80.5",
    env_id == "sexy2-25/26" & crop_id == "a" ~ "94",
    env_id == "sexy2-25/26" & crop_id == "mix" ~ "88",
    env_id == "seed1-25/26" & crop_id == "p" ~ "80.5",

    trt_id == "17_West" & trt_id == "18_West" ~ "Unknown",
    trt_id == "17_East" & trt_id == "18_East" ~ "Unknown",

    TRUE ~ "XX"
  ))


# 6. planting desc --------------------------------------------------------

d6 <-
  d5 %>%
  mutate(planting_desc = case_when(
    env_id == "sexy1-24/25" & crop_id == "p" ~ "Aimed for 300 pl m-2 (1 kg perennial rye contains more seeds than 1 kg annual rye)",
    env_id == "sexy1-24/25" & crop_id == "a" ~ "Aimed for 300 pl m-2",
    env_id == "sexy1-24/25" & crop_id == "mix" ~ "Aimed for 300 pl m-2, 1.4 kg perennial rye for every 1 kg annual rye",

    env_id == "sexy2-25/26" & crop_id == "p" ~ "Aimed for 250 pl m-2",
    env_id == "sexy2-25/26" & crop_id == "a" ~ "Aimed for 250 pl m-2",
    env_id == "sexy2-25/26" & crop_id == "mix" ~ "Aimed for 250 pl m-2",

    trt_id == "17_West" & trt_id == "18_West" ~ "Aimed for 100 pl m-2",
    trt_id == "17_East" & trt_id == "18_East" ~ "Aimed for 300 pl m-2",

    env_id == "seed1-24/25" ~ "Aimed for 300 pl m-2",
    env_id == "seed1-25/26" ~ "Aimed for 300 pl m-2",

    TRUE ~ "XX"
  ))


# 7. row widths -----------------------------------------------------------

#--unsure about Eusun's row spacing EMAILED CASANDRA on 12 dec 2025

d7 <-
  d6 %>%
  mutate(rowspacing_cm = case_when(
    env_id %in% c("sexy1-24/25", "sexy2-25/26") ~ 12.5,
    env_id %in% c("seed1-24/25", "seed1-25/26") ~ 25,
    TRUE ~ NA)
  )


# 8. planting depth -----------------------------------------------------------
#--Confirming with anders and eusun if planted at 3pm depth

d8 <-
  d7 %>%
  mutate(plantingdepth_cm = 3)


# make data ---------------------------------------------------------------

#--at this point all data is from the first stand year. Eusun might share grain from the second stand year in 25/26 season, and we could possibly take some from the seed increase field.

op_trtkey <-
  d8 |>
  select(env_id, trt_id, crop_id, cctrt_id, everything())

usethis::use_data(op_trtkey, overwrite = TRUE)


# make availabel ------------------------------------------------------

op_trtkey |>
  write_csv("inst/extdata/op_trtkey.csv")

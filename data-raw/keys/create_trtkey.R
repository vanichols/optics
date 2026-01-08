
library(readxl)
library(tidyverse)

rm(list = ls())


# trt_key (must be unique to each location)
# env_key (links to a location and growgin season)
# trt_desc (details)
# crop_id (p, a, mix)
# cctrt_id (none, fall mix, NA)
# plantingrate_kgha
# planting_desc (what was target plants per m2, etc)
# row widths, planting depth, planting date
# bio1date_ymd (if biomass was harvested, I think eusun did)
# hdate_ymd (grain harvest date)

#--I don't think I want the details of the treatments kept here,
#--they could differ depending on the trial...


# 1. raw data -------------------------------------------------------------

#--sexy1, handmade
d1a <-
  read_excel("data-raw/keys/sexy1-2024_eukey.xlsx", skip = 5) |>
  select(trt_id) |>
  filter(!is.na(trt_id)) |>
  distinct() |>
  mutate(
    loc_key = "01", #--sexy1
  sea_key = "01", #--24/25
  env_key = paste0(loc_key, sea_key),
  trt_key = paste0(env_key, "_", trt_id )) |>
  select(-trt_id)


#--sexy2
d1b <-
  read_excel("data-raw/keys/sexy2-2025_eukey.xlsx", skip = 5) |>
  select(trt_id = trt) |>
  distinct() |>
  mutate(loc_key = "02", #--sexy2
         sea_key = "02", #--25/26
         env_key = paste0(loc_key, sea_key),
         trt_key = paste0(env_key, "_", trt_id)) |>
  select(-trt_id)


#--seed inc1
d1c <-
tibble(
    loc_key = "03",
    sea_key = "01",
    env_key = paste0(loc_key, sea_key),
    trt_key = paste0(env_key, "_pmechwide24"))

#--eusun1
d1d <-
  read_excel("data-raw/keys/Eusun-treatments-from-Casandra.xlsx", skip = 2) |>
  select(Treatment) |>
  mutate(
    trt_id = str_remove(Treatment, "_"),
    loc_key = "00", #--eusun1
         sea_key = "01", #--24/25
    env_key = paste0(loc_key, sea_key),
    trt_key = paste0(env_key, "_", trt_id)) |>
  select(-trt_id, -Treatment)

#--seed inc, first planting still in place, plus second planting
d1e <-
  #--the trt planted previous year
  tibble(
    loc_key = "03",
    sea_key = "02", #--25/26
    env_key = paste0(loc_key, sea_key),
    trt_key = paste0(env_key, "_pmechwide24")) |>
  #--the trt planted this year
  add_row(loc_key = "03",
          sea_key = "02",
          env_key = "0302",
          trt_key = "0302_pmechwide25")

d1 <-
  d1a |>
  bind_rows(d1b) |>
  bind_rows(d1c) |>
  bind_rows(d1d) |>
  bind_rows(d1e) |>
  select(-loc_key, -sea_key)


# 2. add trt_desc -------------------------------------------------------

d2 <-
  d1 |>
  mutate(trt_desc = case_when(
    trt_key %in% c("0101_p", "0202_p") ~ "Perennial cereal (PC) rye in 12.5 cm rows",
    trt_key %in% c("0101_xp", "0202_xp") ~ "PC rye in 12.5 cm rows without weed control",
    trt_key %in% c("0101_pcc", "0202_pcc") ~ "PC rye in 12.5 cm rows with a cover crop planted after grain harvest",
    trt_key %in% c("0101_xpcc", "0202_xpcc") ~ "PC rye in 12.5 cm rows with a cover crop planted after grain harvest without weed control",

    trt_key %in% c("0101_a", "0202_a") ~ "Annual cereal rye hybrid (A) planted in 12.5 cm rows",
    trt_key %in% c("0101_xa", "0202_xa") ~ "A planted in 12.5 cm rows without weed control",
    trt_key %in% c("0101_acc", "0202_acc") ~ "A planted in 12.5 cm rows with a cover crop planted after grain harvest",
    trt_key %in% c("0101_xacc", "0202_xacc") ~ "A planted in 12.5 cm rows with a cover crop planted after grain harvest without weed control",

    trt_key %in% c("0101_aprows", "0202_aprows") ~ "P and A planted in alternating 12.5 cm rows",
    trt_key %in% c("0101_xaprows", "0202_xaprows") ~ "P and A planted in alternating 12.5 cm rows without weed control",
    trt_key %in% c("0101_apmix", "0202_apmix") ~ "P and A mixture planted in 12.5 cm rows",
    trt_key %in% c("0101_xapmix", "0202_xapmix") ~ "P and A mixture planted in 12.5 cm rows without weed control",

    trt_key == "0301_pmechwide24" ~ "PC rye planted in 25 cm rows with mechanical weed control (for a seed increase) in 2024",
    trt_key == "0302_pmechwide24" ~ "PC rye planted in 25 cm rows with mechanical weed control (for a seed increase) in 2024",
    trt_key == "0302_pmechwide25" ~ "PC rye planted in 25 cm rows with mechanical weed control (for a seed increase) in 2025",
    trt_key == "0001_17West" ~ "PC rye planted in XX cm rows at 1/3 plant density (100 pl m-2), unknown biomass harvest and row width", #---emailed casandra
    trt_key == "0001_18West" ~ "PC rye planted in XX cm rows at 1/3 plant density (100 pl m-2), unknown biomass harvest and row width",
    trt_key == "0001_17East" ~ "PC rye planted in XX cm rows at full plant density (300 pl m-2), unknown biomass harvest and row width",
    trt_key == "0001_18East" ~ "PC rye planted in XX cm rows at full plant density (300 pl m-2), unknown biomass harvest and row width"
  ))

# 3. add crop_id -------------------------------------------------------

#--make a crop_id (a, p, mix)

d3 <-
  d2 |>
  separate(trt_key, into = c("xx", "crop_id"), remove = F) |>
  select(-xx) |>
  mutate(crop_id = case_when(
    crop_id == "p" ~ "p",
    crop_id == "xp" ~ "p",
    crop_id == "pcc" ~ "p",
    crop_id == "xpcc" ~ "p",

    crop_id == "a" ~ "a",
    crop_id == "xa" ~ "a",
    crop_id == "acc" ~ "a",
    crop_id == "xacc" ~ "a",

    crop_id == "aprows" ~ "rowmix",
    crop_id == "xaprows" ~ "rowmix",
    crop_id == "apmix" ~ "mix",
    crop_id == "xapmix" ~ "mix",

    crop_id == "pmechwide24" ~ "p",
    crop_id == "pmechwide25" ~ "p",
    crop_id == "17West" ~ "p",
    crop_id == "18West" ~ "p",
    crop_id == "17East" ~ "p",
    crop_id == "18East" ~ "p",
    TRUE ~ "XXXX"
  ))



# 4. make a cctrt_id ------------------------------------------------------

#--make a cctrt_id (nocc, fcc)


d4 <-
  d3 |>
  mutate(
    cctrt_id = case_when(
      trt_key %in% c("0101_p", "0202_p") ~ "none",
      trt_key %in% c("0101_xp", "0202_xp") ~ "none",
      trt_key %in% c("0101_pcc", "0202_pcc") ~ "fall mix",
      trt_key %in% c("0101_xpcc", "0202_xpcc") ~ "fall mix",

      trt_key %in% c("0101_a", "0202_a") ~ "none",
      trt_key %in% c("0101_xa", "0202_xa") ~ "none",
      trt_key %in% c("0101_acc", "0202_acc") ~ "fall mix",
      trt_key %in% c("0101_xacc", "0202_xacc") ~ "fall mix",

      trt_key %in% c("0101_aprows", "0202_aprows") ~ "none",
      trt_key %in% c("0101_xaprows", "0202_xaprows") ~ "none",
      trt_key %in% c("0101_apmix", "0202_apmix") ~ "none",
      trt_key %in% c("0101_xapmix", "0202_xapmix") ~ "none",

      TRUE ~ NA
    )
  )



# 5. planting rate -----------------------------------------------------------

d5 <-
  d4 %>%
  mutate(plantingrate_kgha = case_when(

    env_key == "0101" & crop_id == "p" ~ "94",
    env_key == "0101" & crop_id == "a" ~ "132",
    env_key == "0101" & crop_id == "mix" ~ "113",
    env_key == "0101" & crop_id == "p" ~ "94",

    env_key == "0301" ~ "94",
    env_key == "0302" ~ "80.5",

    #--lower planting rate in 25/26 season bc was planted earlier
    env_key == "0202" & crop_id == "p" ~ "80.5",
    env_key == "0202" & crop_id == "a" ~ "94",
    env_key == "0202" & crop_id == "mix" ~ "88",
    env_key == "0302" & crop_id == "p" ~ "80.5",

    #--no idea what planting rate eusun used, assume based on our same target plants
    trt_key == "0001_18West" ~ "31",
    trt_key == "0001_17West" ~ "31",
    trt_key == "0001_18East" ~ "94",
    trt_key == "0001_17East" ~ "94",


    TRUE ~ "999"),
    plantingrate_kgha = as.numeric(plantingrate_kgha))


# 6. planting desc --------------------------------------------------------

d6 <-
  d5 %>%
  mutate(planting_desc = case_when(
    env_key == "0101" & crop_id == "p" ~ "Aimed for 300 pl m-2 (1 kg perennial rye contains more seeds than 1 kg annual rye)",
    env_key == "0101" & crop_id == "a" ~ "Aimed for 300 pl m-2",
    env_key == "0101" & crop_id == "mix" ~ "Aimed for 300 pl m-2, 1.4 kg perennial rye for every 1 kg annual rye",

    env_key == "0202" & crop_id == "p" ~ "Aimed for 250 pl m-2",
    env_key == "0202" & crop_id == "a" ~ "Aimed for 250 pl m-2",
    env_key == "0202" & crop_id == "mix" ~ "Aimed for 250 pl m-2",

    trt_key == "0001_17West" & trt_key == "0001_18West" ~ "Aimed for 100 pl m-2",
    trt_key == "0001_17East" & trt_key == "0001_18East" ~ "Aimed for 300 pl m-2",

    env_key == "0301" ~ "Aimed for 300 pl m-2",
    env_key == "0302" ~ "Aimed for 300 pl m-2",

    TRUE ~ "XX"
  ))


# 7. row widths -----------------------------------------------------------

#--unsure about Eusun's row spacing, may have differed by trt? EMAILED CASANDRA on 12 dec 2025

d7 <-
  d6 %>%
  mutate(rowspacing_cm = case_when(
    env_key %in% c("0101", "0202") ~ 12.5,
    env_key %in% c("0301", "0302") ~ 25,
    trt_key == "0001_17West" ~ 9999,
    trt_key == "0001_18West" ~ 9999,
    trt_key == "0001_17East" ~ 9999,
    trt_key == "0001_18East" ~ 9999,
    TRUE ~ NA)
  )


# 8. planting depth -----------------------------------------------------------
#--Confirming with anders and eusun if planted at 3pm depth

d8 <-
  d7 %>%
  mutate(plantingdepth_cm = 3)



# 9. planting dates ----------------------------------------------------------

d9 <-
  d8 %>%
  mutate(pdate_ymd = case_when(
    env_key == "0101" ~ ymd("2024-10-18"), #--sexy1
    env_key == "0301" ~ ymd("2024-10-18"), #--seed inc 1st year
    env_key == "0202" ~ ymd("2025-09-26"), #--sexy2
    env_key == "0302" ~ ymd("2025-09-25"), #--seed inc 2nd year
    env_key == "0001" ~ ymd("2024-12-31"), #--ask casandra
  TRUE ~ ymd("2999-01-01")))

# 10. biomass harvest date(s) ----------------------------------------------------------

d10 <-
  d9 %>%
  mutate(bio1date_ymd = case_when(
    env_key == "0101" ~ NA, #--sexy1
    env_key == "0301" ~ NA, #--seed inc 1st year
    env_key == "0202" ~ NA, #--sexy2
    env_key == "0302" ~ NA, #--seed inc 2nd year
    env_key == "0001" ~ ymd("2025-05-31"), #--ask casandra, this is just a guess
  TRUE ~ ymd("2999-01-01")
    ))

# 11. grain harvest date ----------------------------------------------------------

d11 <-
  d10 %>%
  mutate(hdate_ymd = case_when(
    env_key == "0101" ~ ymd("2025-08-08"), #--sexy1
    env_key == "0301" ~ ymd("2025-08-15"), #--seed inc 1st year
    env_key == "0202" ~ NA, #--sexy2
    env_key == "0302" ~ NA, #--seed inc 2nd year
    env_key == "0001" ~ ymd("2025-08-19"), #--ask casandra, this is just a guess
    TRUE ~ ymd("2999-01-01")
  ))



# 12. add herb_id ---------------------------------------------------------

d12 <-
  d11 |>
  separate(trt_key, into = c("xx", "trt_id"), remove = F) |>
  select(-xx) |>
  select(trt_id, everything()) |>
  mutate(herb_id = case_when(
    trt_id %in% c("a", "p", "apmix", "aprows", "acc", "pcc") ~ "herbicide",
    trt_id %in% c("xa", "xp", "xapmix", "xaprows", "xacc", "xpcc") ~ "herbicide",
    TRUE ~ "UHOH")
  )

# done --------------------------------------------------------------------

op_trtkey <-
  d12 |>
  select(env_key, trt_key, trt_desc, trt_id, crop_id, herb_id,
         everything())

usethis::use_data(op_trtkey, overwrite = TRUE)


op_trtkey |>
  write_csv("inst/extdata/op_trtkey.csv")

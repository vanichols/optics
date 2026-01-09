library(tidyverse)

rm(list = ls())

#--You can periodically dump the data, it saves it continuously

#--installed...maybe 26 march 2025? I didn't mark it down for some reason...

#--you have to just know the names, this is in the manual on page 6
# index number of the measure, date and time in UTC, time zone, T1, T2, T3, soil moisture count, shake,
# errFlag
#--the soil moisture count requires a calibration to be done to relate it to soil volumetric water...
my_names <- c("index", "date_time", "timezone", "tair", "tsurf", "tsoil", "soilmoisture_count", "shake", "errFlag", "unknown")

# function ----------------------------------------------------------------

ProcMoisFiles <- function(f = filename){

  f.sensor_id <- str_extract(basename(f), "\\d+(?=_)")

  d <-
    read_delim(f, col_names = F, delim = ";")

  names(d) <- my_names

  d2 <-
    d |>
    mutate(soilsensor_id = f.sensor_id) |>
    mutate(across(everything(), as.character))

  return(d2)

}


# 1. use the function on the list of files -----------------------------------

# Path to folder of first dump
folder_path <- "data-raw/soilmoisture/20250808/"

# Get all csv files in the folder
files <- list.files(
  path = folder_path,
  pattern = "\\.csv$",
  full.names = TRUE
)

#--make a giant dataframe (inefficient, but it's fine)
a1 <- map_dfr(files, ProcMoisFiles)


# 2. fix the date/time and filter -----------------------------------------

a2 <-
  a1 |>
  mutate(date_time = ymd_hm(date_time, tz = "UTC")) |>
  filter(date_time > ymd("2025-03-26")) |>
  filter(date_time < ymd("2025-08-08"))


# 3. get rid of unneeded cols ---------------------------------------------

a3 <-
  a2 |>
  select(-index, -timezone, -shake, -unknown)


# 4. merge with plot ------------------------------------------------------

load("data/op_soilsensorkey.rda")

a4 <-
  a3 |>
  left_join(op_soilsensorkey) |>
  select(env_key, plot_id, trt_id, everything()) |>
  arrange(plot_id, date_time)


# 5. assign things as numeric ---------------------------------------------

a5 <-
  a4 |>
  mutate(across(c(tair, tsurf, tsoil, soilmoisture_count), as.numeric))

a5 |>
  select(-soilmoisture_count) |>
  pivot_longer(tair:tsoil) |>
  ggplot(aes(date_time, value)) +
  geom_line(aes(color = trt_id, group = plot_id)) +
  facet_grid(name~.)


# 6. take daily averages --------------------------------------------------

a6 <-
  a5 |>
  mutate(date = as_date(date_time)) |>
  group_by(env_key, plot_id, trt_id, date) |>
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))



a6 |>
  select(-soilmoisture_count) |>
  pivot_longer(tair:tsoil) |>
  ggplot(aes(date, value)) +
  geom_line(aes(color = trt_id, group = plot_id)) +
  facet_grid(name~.)


a6 |>
  ggplot(aes(date, soilmoisture_count)) +
  geom_line(aes(color = trt_id, group = plot_id)) +
  facet_grid(.~trt_id)

# write it -------------------------------------------------------------

op_soilsensors <- a6

usethis::use_data(op_soilsensors, overwrite = TRUE)


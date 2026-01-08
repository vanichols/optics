#--stole the cleaned yield data from Emma's sexyRye R package
#--make it into a long dataframe about harvest information
#--grain yield, grain protein, grain moisture, straw yield, straw moisture, other...
#--I did check and my calcs match Emma's final values, so only keeping those


rm(list = ls())
library(tidyverse)

load("data-raw/emma/yield_sexy1.rda")

# load("data/op_envkey.rda")
# load("data/op_trtkey.rda")
# load("data/op_plotkey.rda")


#--ok, some plot halves failed...some entire plots failed. We need to distinguish between those
#--not sure how to deal with this, check them separately
#--which ones failed?
#--
tst <-
  yield_sexy1 |>
  filter(samptype != "y")
#--203, 212, one side of 309


y0 <-
  yield_sexy1 |>
  mutate(env_key = "0101") |>
  select(-trial_loc, -season,
         -harvestdate_year, -harvestdate_month, -harvestdate_day, -harvestdate_id,
         -plothalf_dir,
         -plot_id,
         -croptrt_id,
         -cctrt_id,
         -cropcat,
         -herbtrt_id)



# grain yields ------------------------------------------------------------

y1 <-
  y0 |>
  filter(sheet_name == "20250808_grainyield") |>
  select(-contains("straw"), - contains("lodging"))

y1a <-
  y1 |>
  select(env_key,
         plot_id = plot,
         samptype,
         trt_id,
         grain_yield_dry_Mg_ha = grainyield_dry_ton_ha,
         grain_protein_drybasis_pct = predicted_protein_dry_basis_percent,
         grain_moisture_pct = predicted_vand_percent,
         grain_tkw_drybasis_g = tkw_dry)

#--weird things
y1b <-
  y1a |>
  #--if it failed, ignore the yield
  mutate(grain_yield_dry_Mg_ha = ifelse(samptype == "f", NA, grain_yield_dry_Mg_ha)) |>
  #--not sure why the f plot side of 309 is included, remove it
  filter(!(plot_id == 309 & samptype == "f")) |>
  arrange(plot_id)

#--more weird things
y1c <-
  y1b |>
  #--plot 311 has both a desctructive and a yield side, they are different ylds, dest is higher??
  filter(!(plot_id == "311" & samptype == "y")) |>
  #--same with 409
  filter(!(plot_id == "409" & samptype == "y"))

grain <-
  y1c  |>
  pivot_longer(grain_yield_dry_Mg_ha:grain_tkw_drybasis_g)


# straw ------------------------------------------------------------

y2 <-
  y0 |>
  filter(sheet_name == "20250808_strawyield") |>
  select(-contains("grain"), - contains("lodging"))

y2a <-
  y2 |>
  mutate(
  straw_moisture_pct = straw_moisture_content * 100) |>
  select(env_key,
         plot_id = plot,
         samptype,
         trt_id,
         straw_yield_dry_Mg_ha = strawyield_ton_ha,
         straw_moisture_pct) |>
  arrange(plot_id)

#--no weird things...are the straw yields from the correct half?

straw <-
  y2c  |>
  pivot_longer(straw_yield_dry_Mg_ha:straw_moisture_pct)


# combine -----------------------------------------------------------------

op_yields <-
  grain |>
  bind_rows(straw) |>
  arrange(plot_id)


usethis::use_data(op_yields, overwrite = TRUE)

op_yields %>%
  write_csv("inst/extdata/op_yields.csv")


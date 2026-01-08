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
         grain_Mgha = grainyield_dry_ton_ha,
         gprotein_pct = predicted_protein_dry_basis_percent,
         gmois_pct = predicted_vand_percent,
         tkw_g = tkw_dry)

#--weird things
y1b <-
  y1a |>
  #--if it failed, ignore the yield
  mutate(grain_Mgha = ifelse(samptype == "f", NA, grain_Mgha)) |>
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
  pivot_longer(grain_Mgha:tkw_g)


# straw ------------------------------------------------------------

y2 <-
  y0 |>
  filter(sheet_name == "20250808_strawyield") |>
  select(-contains("grain"), - contains("lodging"))

y2a <-
  y2 |>
  mutate(
  smois_pct = straw_moisture_content * 100) |>
  select(env_key,
         plot_id = plot,
         samptype,
         trt_id,
         straw_Mgha = strawyield_ton_ha,
         smois_pct) |>
  arrange(plot_id)

#--no weird things...are the straw yields from the correct half?

straw <-
  y2a  |>
  pivot_longer(straw_Mgha:smois_pct)


# think about it ----------------------------------------------------------

ylds <-
  grain |>
  bind_rows(straw) |>
  arrange(plot_id)

#--in NA plots of mixes, everything shoudl be NA
ylds1 <-
  ylds |>
  mutate(value = ifelse(samptype == "f", NA, value))


#--write -------------------------------------------------------------------

op_yields <-
  ylds1

usethis::use_data(op_yields, overwrite = TRUE)

op_yields %>%
  write_csv("inst/extdata/op_yields.csv")


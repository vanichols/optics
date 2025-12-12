
library(SEXYrye)
library(tidyverse)


#--get the plots with soil moisture sensors in them

d1 <-
  sexy1_plotkey %>% 
  filter(samptype == "Y") %>% 
  left_join(sexy1_eukey) %>% 
  filter(trt_id %in% c("p", "a")) 


d2 <- 
  d1 %>% 
  mutate(soilsens_id = case_when(
  plot == "101" ~ 94243864,
  plot == "105" ~ 94243868,
  plot == "202" ~ 94243867,
  plot == "209" ~ 94243869,
  plot == "307" ~ 94243865,
  plot == "311" ~ 94243862,
  plot == "405" ~ 94243861,
  plot == "412" ~ 94243866,
  TRUE ~ 99999
  )) %>% 
  select(plot_id, soilsens_id)

d2


sexy1_soilmois <- d2

usethis::use_data(sexy1_soilmois, overwrite = T)

sexy1_soilmois %>% 
  write_csv("inst/extdata/sexy1_soilmois.csv")


library(SEXYrye)
library(tidyverse)

#--get dates I've dumped data

my_files <- list.files("data-raw/soilmoisture/practice/", full.names = T)


#--the names make no sense...
my_names <- c("index", "date", "time", "timezone", "t1", "t2", "t3", "sm_count", "shake", "errFlag")

for (i in length(1:my_files)){
  
  tmp_dat <- read_csv2(my_files[i])
  names(tmp_dat) <- my_names
  tmp_dat
  
}

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


sexy1_soilmoiskey <- d2

usethis::use_data(sexy1_soilmoiskey, overwrite = T)

sexy1_soilmois %>% 
  write_csv("inst/extdata/sexy1_soilmoiskey.csv")

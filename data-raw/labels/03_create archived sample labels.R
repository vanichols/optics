Sys.setenv(LANGUAGE = "en")

#--environment (location plus season)
#--

library(tidyverse)

rm(list = ls())


# use emma's spreadsheet --------------------------------------------------

d0 <- read_excel("data-raw/labels/sexy1-grainlabels-updated-09.03.26.xlsx")

#--add season, what to do with analysis id....
#--gave up for now, need to think more about htis...


write.table(
  l2,
  file = "data-raw/labels/ergot-season01-weights-sexy1.csv",
  sep = ";",
  dec = ",",
  row.names = FALSE
)

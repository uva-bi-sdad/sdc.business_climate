# Count the total employment by geographies

# library -----------------------------------------------------------------------------
library(readr)
library(dplyr)
library(stringr)
library(tigris)
library(sf)
library(data.table)
library(ggplot2)
library(reshape2)
library(crosstable)
library(tidyr)
library(scales)


# upload the data --------------------------------------------------------------------
uploadpath = "Microdata/data/working/"
mi_fairfax_features <-  read_csv(paste0(uploadpath,"mi_fairfax_features_updated.csv.xz"))


# count the total employment by block groups 
fairfax_employment <- mi_fairfax_features %>%
  group_by(geoid,year) %>%
  summarize(total_employment=sum(employment, na.rm=T))


# save the data ---------------------------------------------------------------------------------------
savepath = "Employment/Total/data/distribution/"
readr::write_csv(fairfax_employment, xzfile(paste0(savepath,"va059_bg_mi_",min(fairfax_employment$year),max(fairfax_employment$year),"_total_employment.csv.xz"), compression = 9))


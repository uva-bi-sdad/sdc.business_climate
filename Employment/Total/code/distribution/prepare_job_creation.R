# Count the job creation by geography

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
library(plm)


# upload the data --------------------------------------------------------------------
uploadpath = "Microdata/data/working/"
mi_fairfax_features <-  read_csv(paste0(uploadpath,"mi_fairfax_features_updated.csv.xz"))


# Identify incumbent and new businesses. Count the number of new employees from new business, incumbent.
job_creation <- mi_fairfax_features %>%
  select(duns,year,geoid,entry,employment) %>%
  group_by(duns) %>%
  arrange(desc(year), .by_group=TRUE) %>%
  mutate(employment_diff = c(NA, diff(employment)))%>%
  ungroup() %>%
  filter((entry==1)|(employment_diff>0)) %>%
  group_by(geoid,year) %>%
  summarise(job_creation_new=sum(entry*employment, na.rm=T),
            job_creation_active=sum((1-entry)*employment_diff, na.rm=T),
            business_create_job=length(duns),
            total_job_creation=job_creation_new+job_creation_active,
            perc_job_creation_new=100*job_creation_new/total_job_creation,
            perc_job_creation_active=100*job_creation_active/total_job_creation) %>%
  pivot_longer(!c('geoid','year'), names_to='measure', values_to='value') %>%
  mutate(measure_type = case_when(
          grepl('perc',measure)==T ~ "percentage",
          grepl('job',measure)==T ~ "count"),
         MOE='',
         census_block)

# add geo infos
geofeatures <- block_groups("VA", "059", 2010) %>% 
  select(geoid=GEOID,
         region_name=NAMELSAD) %>%
  mutate(region_type='block group') %>%
  st_drop_geometry()




# save the data ---------------------------------------------------------------------------------------
savepath = "Employment/Total/data/distribution/"
readr::write_csv(job_creation, xzfile(paste0(savepath,"va059_bg_mi_",min(fairfax_employment$year),max(fairfax_employment$year),"_jobs_creation.csv.xz"), compression = 9))


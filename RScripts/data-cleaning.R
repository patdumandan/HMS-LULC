#data cleaning for LULC data####

library(readxl)
library(dplyr)
library(reshape2)
require(standardize)
require(rstanarm)
require(tidyr)

for_dat=read_excel("./Data/MLU-USDA_ERS.xlsx", sheet="forest")%>%mutate(type="forest")
urb_dat=read_excel("./Data/MLU-USDA_ERS.xlsx", sheet="urban")%>%mutate(type="urban")
past_dat=read_excel("./Data/MLU-USDA_ERS.xlsx", sheet="pasture")%>%mutate(type="pasture")
crop_dat=read_excel("./Data/MLU-USDA_ERS.xlsx", sheet="cropland")%>%mutate(type="cropland")
specuse_dat=read_excel("./Data/MLU-USDA_ERS.xlsx", sheet="specialuses")%>%mutate(type="special use")
othr_dat=read_excel("./Data/MLU-USDA_ERS.xlsx", sheet="othrland")%>%mutate(type="other")

full_dat_lulc=rbind(for_dat, urb_dat, past_dat, crop_dat, specuse_dat,othr_dat)

lulc_dat=melt(full_dat_lulc, id=c("type", "Regions and States"))%>%
  rename(year=variable, region=`Regions and States`)%>%
  filter(region=="Northeast")


count_dat=read.csv("./Data/HMScount-1946to2018.csv")
count_dat$year=as.factor(count_dat$year)

full_dat=full_join(lulc_dat, count_dat, by="year")%>%drop_na()%>%
  select(year, species,count, type, value, obs.hours)%>%
  reshape2::dcast(year+species+count+obs.hours~type)%>%
  mutate(cropland=as.numeric(cropland), forest=as.numeric(forest), other=as.numeric(other),
         pasture=as.numeric(pasture), urban=as.numeric(urban),
         year=as.integer(as.character(full_dat$year)))%>%
  select(-`special use`)


str(full_dat)

library(sf)
library(dplyr)
library(here)

data_dir = readLines(here("data_dir.txt"), n=1)

source(here("scripts/convenience_functions.R"))

## Open and clip the relevant FACTS files to CA

facts.fueltrt <- st_read(data("scouting/pseudo-facts/individual/S_USA.Activity_HazFuelTrt_PL.shp"),stringsAsFactors=FALSE)
facts.fueltrt <- facts.fueltrt[facts.fueltrt$ADMIN_REGI == "05",] # restrict to CA
facts.fueltrt <- st_buffer(facts.fueltrt,0)

facts.reforest <- st_read(data("scouting/pseudo-facts/individual/S_USA.Activity_SilvReforestation.shp"),stringsAsFactors=FALSE)
facts.reforest <- facts.reforest[facts.reforest$REGION_COD == "05",] # restrict to CA
facts.reforest <- st_buffer(facts.reforest,0)

facts.tsi <- st_read(data("scouting/pseudo-facts/individual/S_USA.Activity_SilvTSI.shp"),stringsAsFactors=FALSE)
facts.tsi <- facts.tsi[facts.tsi$REGION_COD == "05",] # restrict to CA
facts.tsi <- st_buffer(facts.tsi,0)

facts.harvest <- st_read(data("scouting/pseudo-facts/individual/S_USA.Activity_TimberHarvest.shp"),stringsAsFactors=FALSE)
facts.harvest <- facts.harvest[facts.harvest$ADMIN_REGI == "05",] # restrict to CA
facts.harvest <- st_buffer(facts.harvest,0)

names(facts.reforest)[names(facts.reforest) == "ACTIVITY_N"] <- "ACTIVITY"
names(facts.reforest)[names(facts.reforest) == "METHOD_DES"] <- "METHOD"

names(facts.tsi)[names(facts.tsi) == "ACTIVITY_N"] <- "ACTIVITY"
names(facts.tsi)[names(facts.tsi) == "METHOD_DES"] <- "METHOD"
facts.fueltrt$SUBUNIT_NA <- NA


names(facts.harvest)[names(facts.harvest) == "ACTIVITY_N"] <- "ACTIVITY"
names(facts.harvest)[names(facts.harvest) == "METHOD_DES"] <- "METHOD"

#names(facts.fueltrt)[names(facts.fueltrt) == "SUBUNIT"] <- "SUBUNIT_CN"
names(facts.fueltrt)[names(facts.fueltrt) == "ASU_NBR_UN"] <- "SUBUNIT_SI"
names(facts.fueltrt)[names(facts.fueltrt) == "ASU_UOM"] <- "SUBUNIT_UO"
facts.fueltrt$SUBUNIT_NA <- NA

keep.cols <- c("SUID","ACTIVITY","DATE_AWARD","DATE_COMPL","TREATMENT_","METHOD","NBR_UNITS_","UOM","DATE_PLANN","COST_PER_U","METHOD","EQUIPMENT_","PRODUCTIVI","TREATMENT_","DATA_SOURC","SUBUNIT","DATA_SOU_1","GIS_ACRES","SHAPE_AREA","NBR_UNITS_","UOM","SUBUNIT_SI","SUBUNIT_UO","SUBUNIT_NA")

facts.fueltrt <- facts.fueltrt[,keep.cols]
facts.reforest <- facts.reforest[,keep.cols]
facts.tsi <- facts.tsi[,keep.cols]
facts.harvest <- facts.harvest[,keep.cols]

## Merge the layers
facts <- rbind(facts.fueltrt,facts.reforest,facts.tsi,facts.harvest)

facts = facts %>%
  filter(!(ACTIVITY %in% c("Wildfire - Human Ignition")))

st_write(facts,data("scouting/pseudo-facts/merged/pseudo-facts_merged_full.gpkg"),delete_dsn=TRUE)

## Filter to just 2012 or later

facts_notlarge = facts %>%
  filter(GIS_ACRES < 3000)

st_write(facts_notlarge,data("scouting/pseudo-facts/merged/pseudo-facts_merged_notlarge.gpkg"),delete_dsn=TRUE)

facts_recent = facts_notlarge %>%
  mutate(YEAR_COMPL = stringr::str_sub(DATE_COMPL,1,4)) %>%
  filter(YEAR_COMPL >= 2012)

st_write(facts_recent,data("scouting/pseudo-facts/merged/pseudo-facts_merged_2012onward.gpkg"), delete_dsn = TRUE)

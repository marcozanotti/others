# Data joining
source("R/libraries.R")
source("R/functions.R")

feats <- read_excel("data/usa_features.xlsx")
vols <- read_excel("data/usa_volumes.xlsx")

db_polls <- states_merge(states_dir = c("data/september", "data/october"),  features_db = feats)
any(is.na(db_polls))

db_polls <- volumes_merge(db_polls, vols)
any(is.na(db_polls))

db_polls <- compute_margin(db_polls)
any(is.na(db_polls))
save(db_polls, file = "data/db_polls.RData")


usa_geom_feats <- usa_geomdb(feats)
any(is.na(usa_geom_feats))
save(usa_geom_feats, file = "data/usa_geomfeatures.RData")


# Logo
# image_read("data/logo/T-voice_lightBKG.png") %>% 
#   image_trim() %>% 
#   image_write("data/logo/logonew.png")

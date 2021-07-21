# Generate habitat-level pseudo quads
library(tidyverse)
rm(list=ls())

all_nvc <- read_csv("data/NVC_Floristic_Table.csv")
all_nvc <- all_nvc[, -1] # Do not need row number


# Extract out and remove rows containing spp numbers
# Several NVCs, e.g. MG7, do not have a 'main' community, only sub-communities
# Some only have a main community, e.g. S3
# Treat these the same as anything else for now
# Not all communities have estimated numbers of spp per sample
# None of OV communities have estimated numbers of spp per sample in spreadsheet
# although they are present in handbooks. Do not use this information for now.
# Whilst it may give more realistic pseudoquads, will such info be available
# in international comparators? i.e. need to have as generalisable a method as
# possible.
# S4e and S4f do not have sub-community names in spreadsheet; Correct them here:
all_nvc$`Community or sub-community name`[all_nvc$`Community or sub-community code` == "S4e"] <-
  "Phragmites australis S4e" # Once book online add correct sub-community
all_nvc$`Community or sub-community name`[all_nvc$`Community or sub-community code` == "S4f"] <-
  "Phragmites australis S4f" # Once book online add correct sub-community

# Cannot drop sub-communities given some lack main community.
# Do not use spp_per_sample due to inconsistencies in database and lack of
# generalisability to international classifications

all_nvc <- filter(all_nvc, !`Species name or special variable` == "Number of species per sample")
all_nvc <- filter(all_nvc, !`Species name or special variable` == "Vegetation height cm")
all_nvc <- filter(all_nvc, !`Species name or special variable` == "Mean total cover")
all_nvc <- filter(all_nvc, !is.na(`Maximum abundance of species`))
all_nvc <- select(all_nvc, -`Special variable value`)
all_nvc <- mutate(all_nvc, habitat = sub("^([[:alpha:]]*).*", "\\1",
                                           all_nvc$`Community level code`))
all_nvc <- mutate(all_nvc,
                  const_prob = case_when(`Species constancy value` == "I" ~ 0.2,
                                         `Species constancy value` == "II" ~ 0.4,
                                         `Species constancy value` == "III" ~ 0.6,
                                         `Species constancy value` == "IV" ~ 0.8,
                                         `Species constancy value` == "V" ~ 1))


# Create random main habitats; done at NVC main level but will be grouped at
# habitat level for ordination purposes
no_of_nvcs <- length(unique(all_nvc$`Community or sub-community code`))
no_of_habitats <- length(unique(all_nvc$habitat))

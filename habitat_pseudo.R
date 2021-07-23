# Generate habitat-level pseudo quads
library(tidyverse)
library(readxl)

rm(list=ls())

all_nvc <- read_excel("data/NVC-floristic-tables.xls",
                      col_types = c("numeric", rep("text", 5), rep("numeric", 2)),
                      range = cell_cols("A:H"))
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

# Cannot drop sub-communities given some lack main community.
# Do not use spp_per_sample due to inconsistencies in database and lack of
# generalisability to international classifications

all_nvc <- filter(all_nvc, is.na(`Special variable value`))
all_nvc <- all_nvc[, -7]
all_nvc <- mutate(all_nvc, habitat = sub("^([[:alpha:]]*).*", "\\1",
                                           all_nvc$`Community level code`))
all_nvc <- mutate(all_nvc,
                  const_prob = case_when(`Species constancy value` == "I" ~ 0.2,
                                         `Species constancy value` == "II" ~ 0.4,
                                         `Species constancy value` == "III" ~ 0.6,
                                         `Species constancy value` == "IV" ~ 0.8,
                                         `Species constancy value` == "V" ~ 1))
all_nvc <- all_nvc[,- 5]
# Simplify names for ease of coding
colnames(all_nvc) <- c("full_nvc_code", "nvc_name", "comm_level_code",
                       "spp_name", "domin", "habitat", "max_const")

# Create random main habitats; done at NVC main level but will be grouped at
# habitat level for ordination purposes
no_of_nvcs <- length(unique(all_nvc$comm_level_code))
no_of_habitats <- length(unique(all_nvc$habitat))
habitat_names  <- unique(all_nvc$habitat)

# Begin by creating 3 pseudo-quads for each habitat, select a community at 
# random for a given habitat
set.seed(123)
pseudos_per_habitat <- 3
for(this_habitat in habitat_names){
  print(this_habitat)
  # Select NVC at random within that habitat
  data_this_habitat <- filter(all_nvc, habitat == this_habitat)
  nvc_names_this_habitat <- unique(data_this_habitat$full_nvc_code)
  # print(nvc_names_this_habitat)
  rnd_nvc_names <- sample(nvc_names_this_habitat, pseudos_per_habitat, replace = TRUE)
  
  # Generate a random quadrat for each habitat in rnd_nvc_names
  for(rnd_nvc in rnd_nvc_names){
    rnd_nvc_record <- filter(data_this_habitat, full_nvc_code == rnd_nvc)
    print(rnd_nvc_record)
    const_prob <- runif(1)
    print(const_prob)
    if(const_prob >= 0.8 ){
      print("Need constancy level V")
    } else if (const_prob >= 0.6 && const_prob < 0.8){
      print("Need constancy level IV")
    } else if (const_prob >= 0.4 && const_prob < 0.6){
      print("Need constancy level III")
    } else if (const_prob >= 0.2 && const_prob < 0.4){
      print("Need constancy level II")
    } else if (const_prob < 0.2){
      print("Need constancy level I")
    }
  }
}

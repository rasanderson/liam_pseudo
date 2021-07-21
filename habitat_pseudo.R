# Generate habitat-level pseudo quads
library(tidyverse)

all_nvc <- read_csv("data/NVC_Floristic_Table.csv")
all_nvc <- all_nvc[, -1] # Do not need row number

# Drop sub-communities
communities_only <- !str_detect(all_nvc$`Community or sub-community name`, "sub-community")
main_nvc <- all_nvc[communities_only,]

# Extract out and remove rows containing spp numbers
# NOTE: NEED TO FIX MG7 AND SM12
spp_per_nvc <- filter(main_nvc, `Species name or special variable` == "Number of species per sample")
spp_per_nvc <- spp_per_nvc[, -c(4,5,6)]
colnames(spp_per_nvc)[4] <- "spp_per_sample"
main_nvc <- filter(main_nvc, !`Species name or special variable` == "Number of species per sample")
main_nvc <- filter(main_nvc, !`Species name or special variable` == "Vegetation height cm")
main_nvc <- filter(main_nvc, !`Species name or special variable` == "Mean total cover")
main_nvc <- filter(main_nvc, !is.na(`Maximum abundance of species`))
main_nvc <- select(main_nvc, -`Special variable value`)
main_nvc <- mutate(main_nvc, habitat = sub("^([[:alpha:]]*).*", "\\1", main_nvc$`Community level code`))

# Create random main habitats; done at NVC main level but will be grouped at
# habitat level for ordination purposes
no_of_nvcs <- nrow(spp_per_nvc)

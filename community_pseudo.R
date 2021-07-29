# Generate community-level pseudo quads based on results of habitat_pseudo.R
# This selected H, U and M as the top three habitats, so focus on them.
library(tidyverse)
library(readxl)
set.seed(123)

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
all_nvc <- filter(all_nvc, habitat %in% c("U", "M", "H"))


# Create random main habitats; done at NVC main level but will be grouped at
# habitat level for ordination purposes
no_of_nvcs <- length(unique(all_nvc$comm_level_code))
no_of_habitats <- length(unique(all_nvc$habitat))
nvc_names  <- unique(all_nvc$comm_level_code)

# Generation of pseudoquads at community level commented out for speed
# # Begin by creating a few pseudo-quads for each habitat, select a community at 
# # random for a given habitat
pseudos_per_community <- 150
# pseudoquad_data <- NULL
# pseudoquad_no   <- 1
# for(this_community in nvc_names){ # Each habitat
#   # Select NVC at random within that habitat
#   data_this_community <- filter(all_nvc, comm_level_code == this_community)
#   nvc_names_this_community <- unique(data_this_community$full_nvc_code)
#   # print(nvc_names_this_habitat)
#   rnd_nvc_names <- sample(nvc_names_this_community, pseudos_per_community, replace = TRUE)
#   
#   # Generate a random quadrat for each habitat in rnd_nvc_names
#   for(rnd_nvc in rnd_nvc_names){ # randomly selected NVC in habitat
#     rnd_nvc_record <- filter(data_this_community, full_nvc_code == rnd_nvc)
#     rnd_nvc_record <- mutate(rnd_nvc_record, spp_chosen = FALSE)
#     rnd_nvc_record <- mutate(rnd_nvc_record, pseudo_code = pseudoquad_no)
#     const_prob <- runif(1)
#     pct_cover <- 0
#     print(paste0("Select random NVC: ", rnd_nvc))
#     while(pct_cover < 100){
#       if(all(rnd_nvc_record$spp_chosen) == TRUE){
#         pct_cover <- 100
#       }
#       rnd_spp_no <- sample(1:nrow(rnd_nvc_record), 1)
#       if(rnd_nvc_record$spp_chosen[rnd_spp_no] == FALSE){
#         const_prob <- runif(1)
#         if(const_prob <= rnd_nvc_record$max_const[rnd_spp_no]){
#           cover_rnd_pct <- runif(1) * 100
#           spp_pct_max <- rnd_nvc_record$domin[rnd_spp_no]^2.6/4 # Currall Domin 2.6
#           if(cover_rnd_pct <= spp_pct_max){
#             rnd_nvc_record$spp_chosen[rnd_spp_no] <- TRUE
#             pred_spp_cover <- cover_rnd_pct
#             pct_cover <- pct_cover + pred_spp_cover
#             pseudo_record <- data.frame(psuedo_id = pseudoquad_no,
#                                         spp_id = rnd_nvc_record$domin[rnd_spp_no],
#                                         spp_name = rnd_nvc_record$spp_name[rnd_spp_no],
#                                         spp_pct = pred_spp_cover,
#                                         nvc_code = rnd_nvc,
#                                         community_code = this_community)
#             pseudoquad_data <- rbind(pseudoquad_data, pseudo_record)
#           } # End pct cover
#         } # End constancy
#       } # End check on with critical constancy reached
#     } # End pseudoquad
#     pseudoquad_no <- pseudoquad_no + 1
#   }
# }

# It takes about 20 minutes to generate the pseudoquads so skip above and read
# previously saved results for speed.
pseudoquad_data <- read.csv("outputs/community_pseudoquads.csv")

# Cannot implement make.cepnames until turned into wide format
library(vegan)
pseudoquad_data2 <- pseudoquad_data[, c("psuedo_id", "spp_name", "spp_pct")]
pseudoquad_data_wde <- pivot_wider(pseudoquad_data2, values_from = spp_pct,
                                   names_from = spp_name,
                                   values_fill = 0)
#pseudoquad_data_wde <- pseudoquad_data_wde[, -1]
pseudoquad_data_wde <- pseudoquad_data_wde[, sort(colnames(pseudoquad_data_wde))]
colnames(pseudoquad_data_wde) <- make.cepnames(colnames(pseudoquad_data_wde))

# 
# library(umap)
# pseudoquad_data_wde <- decostand(pseudoquad_data_wde, method="standardize")
# pseudo_umap <- umap(pseudoquad_data_wde, random_state=123)
# pseudo_umap_lyt <- data.frame(pseudo_umap$layout)
# pseudo_umap_lyt <- cbind(pseudo_umap_lyt, rep(nvc_names, each = pseudos_per_community))
# colnames(pseudo_umap_lyt) <- c("umap1", "umap2", "community")

library(uwot)
pseudoquad_data_wde <- decostand(pseudoquad_data_wde, method="pa")
pseudo_umap <- umap(pseudoquad_data_wde, pca=100, n_components=3, n_threads=4)
pseudo_umap_lyt <- data.frame(pseudo_umap)
pseudo_umap_lyt <- cbind(pseudo_umap_lyt, rep(nvc_names, each = pseudos_per_community))
colnames(pseudo_umap_lyt) <- c("umap1", "umap2", "umap3", "community")


library(ggplot2)
ggplot(pseudo_umap_lyt, aes(x = umap1, y = umap2, colour = community)) +
  geom_point(size = 3) +
  #scale_shape_manual(values = 11:22) +
  theme_classic()

ggplot(pseudo_umap_lyt, aes(x = umap1, y = umap3, colour = community)) +
  geom_point(size = 3) +
  #scale_shape_manual(values = 11:22) +
  theme_classic()

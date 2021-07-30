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
pseudo_umap <- umap(pseudoquad_data_wde, pca=100, ret_model = TRUE,
                    y = as.factor(rep(nvc_names, each = pseudos_per_community)))
pseudo_umap_lyt <- data.frame(pseudo_umap$embedding)
pseudo_umap_lyt <- cbind(pseudo_umap_lyt, rep(nvc_names, each = pseudos_per_community))
colnames(pseudo_umap_lyt) <- c("umap1", "umap2", "community")


library(ggplot2)
ggplot(pseudo_umap_lyt, aes(x = umap1, y = umap2, colour = community)) +
  geom_point(size = 3) +
  #scale_shape_manual(values = 11:22) +
  theme_classic()

community_umap <- pseudo_umap_lyt %>% 
  group_by(community) %>% 
  summarise(mean_umap1 = mean(umap1), mean_umap2 = mean(umap2),
            se_umap1 = sd(umap1)/sqrt(pseudos_per_community),
            se_umap2 = sd(umap2)/sqrt(pseudos_per_community),
            ci_umap1 = qt(0.05/2, pseudos_per_community-1, lower.tail=FALSE) * se_umap1,
            ci_umap2 = qt(0.05/2, pseudos_per_community-1, lower.tail=FALSE) * se_umap2)

ggplot(community_umap, aes(x = mean_umap1, y = mean_umap2, colour = community)) +
  geom_point(size = 3) +
  #geom_errorbar(aes(xmin = mean_umap1 - se_umap1, xmax = mean_umap1 + se_umap1)) +
  #geom_errorbar(aes(ymin = mean_umap2 - se_umap2, ymax = mean_umap2 + se_umap2)) +
  #scale_shape_manual(values = 11:22) +
  theme_classic()



# Check the Ashtrees data
ash <- read.csv("data/Ashtrees_perc_final.csv")
ash <- ash[, -1]
# Ensure abbreviated names match
# pseudonam <- colnames(pseudoquad_data_wde)
# View(sort(pseudonam))
# ashnam <- colnames(ash)
# View(sort(ashnam))
# Agrostis_spp to Agrocapi in Ashtrees;
#   Recode to Agrocapi adding values together
ash[, "Agrocapi"] <- ash[, "Agrocapi"] + ash[, "Agrostis_spp"]
ash <- subset(ash, select = -Agrostis_spp)
# Carepani listed as Carepani and Carepani.1 in pseudos and Carepanic in ash;
#   pseudos Carepani=Carex panicea, Carepani.1=Carex paniculata; ash is panicea
colnames(ash)[colnames(ash)=="Carepanic"] <- "Carepani"
# Carex_spp needs changing;
#   Only one record with low abundance so delete
ash <- subset(ash, select = -Carex_spp)
# Dricscop missing from pseudos. Check name change
#   This is actually Dicranum scopularium so recode to Dicrscop
colnames(ash)[colnames(ash)=="Dricscop"] <- "Dicrscop"
# Durodili missing from pseudos. Check name change
#   Recode to Dryodila
colnames(ash)[colnames(ash)=="Durodili"] <- "Dryodila"
# Festrubr missing from pseudos. Check name change or Festagg
#   Unclear why not in pseudos. Consider recoding to Festagg or Festovin
# colnames(ash)[colnames(ash)=="Festrubr"] <- "Festagg"
ash[, "Festovin"] <- ash[, "Festovin"] + ash[, "Festrubr"]
ash <- subset(ash, select = -Festrubr)
# Hylosple and Hylosple.1 in Ashtrees
#   Add Hylosple.1 to Hylosple in Ashtrees
ash[, "Hylosple"] <- ash[, "Hylosple"] + ash[, "Hylosple.1"]
ash <- subset(ash, select = -Hylosple.1)
# Spelling on Hypnjutu Hypnjutl
#   Recode in Ashtrees to Hypnjutl
colnames(ash)[colnames(ash)=="Hypnjutu"] <- "Hypnjutl"
# Luzumult in pseudo and Luzumulti in ash
#   Recode to Luzumult in ash
colnames(ash)[colnames(ash)=="Luzumulti"] <- "Luzumult"
# Plagundu listed as Plagiotundu in ash
#   Recode to Plagundu
colnames(ash)[colnames(ash)=="Plagiotundu"] <- "Plagundu"
# Poaprat missing from pseudos. Check name change
#   Exists in NVC floristics table but not selected. Delete from Ashtrees
ash <- subset(ash, select = -Poaprat)
# Rumex acetosa and acetosella need checking
#   Put pseudos alphabetically before cepname to avoid ambiguity
#   In pseudos Rumeacet=Rumex acetosa, Rumeacet.1=Rumex acetosella
#   Ash already in this order but needs abbreviating
colnames(ash)[colnames(ash)=="Rumeacetosa"] <- "Rumeacet"
colnames(ash)[colnames(ash)=="Rumeacetosella"] <- "Rumeacet.1"
# Sphafall not in pseudos. Check name change
#   Recode to Spharecu.
colnames(ash)[colnames(ash)=="Sphafall"] <- "Spharecu"
# sphapalu lower case s in Ashtrees
#   Reocde to Sphapalu
colnames(ash)[colnames(ash)=="sphapalu"] <- "Sphapalu"
# Stelalsi not in pseudos but check Stelagg
#   Only couple of records and delete from Ashtrees
ash <- subset(ash, select = -Stelalsi)
# Taraxacu needs checking in ash and pseudos
#   Presumsably Taraoffi but not clear why coded differently. Safer to omit couple of records.
ash <- subset(ash, select = -Taraxacu)

# To make a umap prediction, colnames must be identical else error of wrong
# number of rownames(!) is generated.
pseudo_not_ash <- !(colnames(pseudoquad_data_wde) %in% colnames(ash))
ash_not_pseudo <- colnames(ash) %in% colnames(pseudoquad_data_wde)

pseudo_not_ash_df <- data.frame(matrix(0,
                                       nrow=nrow(ash),
                                       ncol=as.numeric(summary(pseudo_not_ash)[3]))
)
colnames(pseudo_not_ash_df) <- colnames(pseudoquad_data_wde)[pseudo_not_ash]
ash_pseudo <- cbind(ash, pseudo_not_ash_df)
ash_pseudo <- ash_pseudo[, sort(colnames(ash_pseudo))]
ash_pseudo <- decostand(ash_pseudo, method="pa")

#ash_pred <- predict(pseudo_umap, ash_pseudo)
ash_transform <- umap_transform(ash_pseudo, pseudo_umap, verbose=TRUE)
ash_pred <- data.frame(ash_transform)
colnames(ash_pred) <- c("mean_umap1", "mean_umap2") # Not means, but matches plot
ash_pred <- data.frame(ash_pred)
ash_pred <- mutate(ash_pred, community="Pred")


habitat_plt <- ggplot(community_umap, aes(x = mean_umap1, y = mean_umap2, colour = community)) +
  geom_point(size = 3.5) +
  geom_point(data=ash_pred, aes(x=mean_umap1, y=mean_umap2), size = 1, color="black") +
  #geom_errorbar(aes(xmin = mean_umap1 - ci_umap1, xmax = mean_umap1 + ci_umap1)) +
  #geom_errorbar(aes(ymin = mean_umap2 - ci_umap2, ymax = mean_umap2 + ci_umap2)) +
  scale_shape_manual(values = 11:23) #+
#theme_classic()
habitat_plt



# Check on similarity to main habitats
ash_probs <- NULL
for(ash_quad in 1:nrow(ash_pred)){
  this_quad <- ash_quad
  # Distance from centroids
  dist_to_nvcs <- NULL
  prob_to_nvcs <- NULL
  for(nvc in 1:no_of_nvcs){
    dist <- 0
    dist <- dist + (community_umap$mean_umap1[nvc] - ash_pred$mean_umap1[this_quad])^2
    dist <- dist + (community_umap$mean_umap2[nvc] - ash_pred$mean_umap2[this_quad])^2
    dist <- sqrt(dist)
    dist_to_nvcs <- rbind(dist_to_nvcs,
                              data.frame(quad=this_quad,
                                         nvc_nam=community_umap$community[nvc],
                                         distance=dist))
  }
  # Similarity score
  invsum_dist <- 0
  for(nvc in 1:no_of_nvcs){
    invsum_dist <- invsum_dist + 1/dist_to_nvcs$distance[nvc]
  }
  probs <- NULL
  for(nvc in 1:no_of_nvcs){
    prob <- (1/dist_to_nvcs$distance[nvc]) / invsum_dist 
    probs <- rbind(probs, prob)
  }
  dist_to_nvcs <- cbind(dist_to_nvcs, probs[,1])
  colnames(dist_to_nvcs)[4] <- "probability"
  ash_probs <- rbind(ash_probs, dist_to_nvcs)
}
ash_probs <- subset(ash_probs, select = -distance)

# Now need to see what are the top probabilities for each quadrat
top1 <- ash_probs %>% 
  group_by(quad) %>% 
  arrange(quad, desc(probability)) %>% 
  top_n(1) %>% 
  ungroup() 
summary(as.factor(top1$nvc_nam))
